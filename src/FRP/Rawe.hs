{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs,
  FunctionalDependencies, OverloadedStrings,
  TypeSynonymInstances, NoMonomorphismRestriction #-}

{-# LANGUAGE DoRec #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}


module FRP.Rawe where


import Prelude hiding (head,div,(.),id,fst,snd,span)

import Control.Category
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Functor

import Control.Monad.State
import Control.Monad.Writer

import Data.String
import Data.Void

import Text.JSON


import Debug.Trace


data Attribute = AttrVal String String
               | AttrBool String

class Attributable b where
        (!) :: b -> Attribute -> b


newtype RawJS = RawJS { unRawJS :: String }
instance IsString RawJS where fromString = RawJS



class BhvValue a where
        bhvValue :: a -> HtmlM RawJS

instance BhvValue () where bhvValue () = return "cthunk([])"
instance BhvValue Int where bhvValue x = return.RawJS $ "cthunk("++show x++")"
instance BhvValue Bool where bhvValue x = return $ if x then "cthunk(true)" else "cthunk(false)"

instance BhvValue Attribute where
        bhvValue (AttrVal name value) = do
                (RawJS jn) <- bhvValue name; (RawJS jv) <- bhvValue value
                return.RawJS $ "cthunk( { AttrVal: ["++jn++","++jv++"] } )"
        bhvValue (AttrBool name) = do
                (RawJS jn) <- bhvValue name
                return.RawJS $ "cthunk( { AttrBool: ["++jn++"] } )"


instance BhvValue Html where
    bhvValue html = do
        (i, (raw, ())) <- withHtmlCurrent $ renderH html
        modify $ \s -> s { hsHtmlValues = (i, raw, Nothing) : hsHtmlValues s }
        return.RawJS $ "cthunk(r_html_"++show i++")"

instance BhvValue (HtmlM (Behaviour a)) where
    bhvValue html = do
        (i, (raw, b)) <- withHtmlCurrent $ renderH html
        (RawJS inner) <- bhvValue b
        modify $ \s -> s { hsHtmlValues = (i, raw, Just inner) : hsHtmlValues s }
        return.RawJS $ "cthunk(r_html_"++show i++")"


withHtmlCurrent :: HtmlM a -> HtmlM (Int, a)
withHtmlCurrent act = do
    i <- htmlUniq
    orig <- get
    put $ orig { hsHtmlCurrent = Just i }
    res <- act
    modify $ \s -> s { hsHtmlCurrent = hsHtmlCurrent orig }
    return (i, res)


jsHtml :: HtmlM a -> HtmlM (RawJS, a)
jsHtml html = do
        (raw, x) <- renderH html
        return (RawJS $ "$('"++escapeStringJS raw++"')", x)

instance BhvValue Char where
        bhvValue x = return.RawJS $ "cthunk("++show x++")"

instance (BhvValue a, BhvValue b) => BhvValue (a, b) where
        bhvValue (x,y) = do
                (RawJS x') <- bhvValue x; (RawJS y') <- bhvValue y
                return.RawJS $ "cthunk(["++x'++", "++y'++"])"

instance (BhvValue a) => BhvValue (Maybe a) where
        bhvValue (Just x) = do (RawJS jx) <- bhvValue x
                               return.RawJS $ "cthunk({Just:"++jx++"})"
        bhvValue Nothing  = return.RawJS $ "cthunk({Nothing:null})"

instance BhvValue a => BhvValue [a] where
        bhvValue [] = return.RawJS $ "cthunk({nil:[]})"
        bhvValue (x:xs) = do RawJS jx <- bhvValue x
                             RawJS jxs <- bhvValue xs
                             return.RawJS $ "cthunk({cons:["++jx++","++jxs++"]})"

{-
instance (BhvValue a) => BhvValue (Timed a) where
        bhvValue _ = error "Time is a phantom type"
        bhvValue (Timed t x) = do (RawJS jt) <- bhvValue t
                                  (RawJS jx) <- bhvValue x
                                  return.RawJS $ "{Timed:["++jt++","++jx++"]}"
        bhvValue NotYet = return.RawJS $ "{NotYet:null}"
        -}

instance BhvValue JSString where
        bhvValue str = return.RawJS $ "cthunk('"++escapeStringJS (fromJSString str)++"')"


escapeStringJS = (>>=helper)
        where helper '\n' = "\\n"
              helper c | c `elem` "'\"\\" = '\\':c:[]
                       | otherwise        = [c]

escapeStringHtml = (>>=helper)
        where helper '"' = "&quot;"
              helper '&' = "&amp;"
              helper '<' = "&lt;"
              helper '>' = "&gt;"
              helper c = [c]


data BhvFun a b = forall f. (BhvPrim f a b) => Prim f
                | Assigned (Int, Int)
                | (a ~ b) => BhvID

type Bhv a = BehaviourFun () a

type BehaviourFun = BhvFun
type Behaviour a = Bhv a





instance Category BehaviourFun where
        id = BhvID
        BhvID . b = b
        b . BhvID = b
        x@(Prim _) . y@(Prim _) = Prim $ BhvModifier2 "compose" y x
        x@(Prim _) . y@(Assigned _) = Prim $ BhvModifier2 "compose" y x
        x@(Assigned _) . y@(Prim _) = Prim $ BhvModifier2 "compose" y x
        x@(Assigned _) . y@(Assigned _) = Prim $ BhvModifier2 "compose" y x


instance PFunctor (,) BehaviourFun BehaviourFun where
    first = first'

instance QFunctor (,) BehaviourFun BehaviourFun where
    second = second'

instance Bifunctor (,) BehaviourFun BehaviourFun BehaviourFun where
    bimap = bimapPreCartesian

instance Braided BehaviourFun (,) where
    braid = braidPreCartesian

instance Associative BehaviourFun (,) where
    associate = associatePreCartesian

instance Coassociative BehaviourFun (,) where
    coassociate = coassociatePreCartesian

instance PreCartesian BehaviourFun (,) where
        x &&& y = Prim $ BhvModifier2 "product" x y
        fst = primFunc "fst"
        snd = primFunc "snd"

instance HasIdentity BhvFun (,) Void

instance Monoidal BhvFun (,) Void where
    idl = snd
    idr = fst

instance CCC BehaviourFun (,) (->) Void where
    apply = primFunc "apply"
    curry = Prim . BhvModifier "curry"
    uncurry = Prim . BhvModifier "uncurry"


bhvPack :: BhvFun a (Bhv a)
bhvPack = primFunc "bhv_pack"

bhvUnpack :: BhvFun (Bhv a) a
bhvUnpack = primFunc "bhv_unpack"


instance (BhvValue b) => BhvValue (Bhv a -> b) where
    bhvValue = bhvValueCommon bhvValue $ \r iid ->
        [ "var r_bhv_fun_"++show r++" = {};"
        , "r_bhv_fun_"++show r++"["++show iid++"] = param.get();"
        ]
        {-
        code <- htmlLocal $ do
            iid <- htmlUniq
            r <- gets hsRecursion
            RawJS result <- bhvValue $ f (Assigned (r,iid))

            bs <- gets $ reverse.hsBehaviours
            hs <- gets $ reverse.hsHtmlValues
            hbs <- gets $ reverse.hsHtmlBehaviours
            return $
                "\tvar r_bhv_fun_"++show r++" = {};\n" ++
                "\tr_bhv_fun_"++show r++"["++show iid++"] = param.get();\n" ++
                (concat $ concat $
                [ flip map bs $ \((r',id'), func, params) ->
                    "\tr_bhv_fun_"++show r'++"["++show id'++"] = new BhvFun("++show id'++");\n"
                , flip map hs $ \(i, h) ->
                    "var r_html_"++show i++" = $('"++escapeStringJS h++"');\n"
                , flip map hbs $ \(i, mv) ->
                    let var = case mv of Nothing -> "$('body')"
                                         Just v  -> "r_html_"++show v
                     in "r_bhv_fun_"++show r++"["++show i++"].html = "++var++".find('*[bhv-id="++show i++"]');\n"
                , flip map bs $ \((r',id'), func, params) ->
                    let jid = show id'
                        jfunc = "r_prim_"++func
                        jparams = concatMap ((',':).unRawJS) params
                     in "\t"++jfunc++".call(r_bhv_fun_"++show r'++"["++jid++"]"++jparams++");\n"
                ]) ++ "\treturn "++result++";\n"

        return $ RawJS $ "cthunk(function(param) {\n" ++ code ++ "})"
        -}


class BhvValueFun a b | a -> b where
    bhvValueFun :: a -> HtmlM RawJS
    cbf :: a -> Bhv b

instance BhvValueFun (Bhv a) a where
    bhvValueFun x = do RawJS jx <- bhvValue x
                       return $ RawJS (jx ++ ".get().compute(cthunk(null))")
    cbf = id

instance BhvValueFun b b' => BhvValueFun (Bhv a -> b) (a -> b') where
    bhvValueFun = bhvValueCommon bhvValueFun $ \r iid ->
        [ "var r_bhv_fun_"++show r++" = {};"
        , "r_bhv_fun_"++show r++"["++show iid++"] = new BhvFun();"
        , "r_prim_const.call(r_bhv_fun_"++show r++"["++show iid++"], param);"
        ]
    cbf = Prim . BhvConstFun

bhvValueCommon bv begin f = htmlLocal $ do
    iid <- htmlUniq
    r <- gets hsRecursion
    RawJS result <- bv $ f (Assigned (r,iid))

    bs <- gets $ reverse.hsBehaviours
    hs <- gets $ reverse.hsHtmlValues
    hbs <- gets $ reverse.hsHtmlBehaviours
    hgs <- gets $ reverse.hsHtmlGens

    return $ RawJS $ init $ unlines $
        ("cthunk(function(param) {":) $ (++[replicate (r-1) '\t' ++ "})"]) $
        map (replicate r '\t' ++) $ (begin r iid ++) $
        concat
        [ flip map bs $ \(id, func, params) ->
            "r_bhv_fun_"++show r++"["++show id++"] = new BhvFun("++show id++");"

        , flip map hs $ \(i, h, mi) ->
            "var r_html_"++show i++" = $('"++escapeStringJS h++"')" ++
            case mi of { Nothing -> ""; Just inner -> ".prop('rawe_html_inner', "++inner++")" }
            ++ ";"

        , flip map hbs $ \(i, mv) ->
            let var = case mv of Nothing -> "$('body')"
                                 Just v  -> "r_html_"++show v
             in "r_bhv_fun_"++show r++"["++show i++"].html = "++var++".find('*[bhv-id="++show i++"]');"

        , flip map hgs $ \(i, mv) ->
            let var = case mv of Nothing -> "$('body')"
                                 Just v  -> "r_html_"++show v
             in "r_bhv_fun_"++show r++"["++show i++"].gen = "++var++".find2('*[bhv-gen="++show i++"]');"

        , flip map bs $ \(id, func, params) ->
            let jid = show id
                jfunc = "r_prim_"++func
                jparams = concatMap ((',':).unRawJS) params
             in jfunc++".call(r_bhv_fun_"++show r++"["++jid++"]"++jparams++");"

        , flip map hbs $ \(i, _) ->
            "r_bhv_fun_"++show r++"["++show i++"].invalidate();"
        ]
        ++
        [ "return "++result++";" ]


data BhvConstFun a b b' = (BhvValueFun b b') => BhvConstFun b
instance BhvPrim (BhvConstFun a b b') a b' where
        bhvPrim (BhvConstFun x) = do jx <- bhvValueFun x
                                     return ("const", [jx])

--cbf :: (BhvValueFun a a') => a -> BhvFun b a'
--cbf = Prim . BhvConstFun


haskToBhv :: (Bhv a -> Bhv b) -> BhvFun a b
haskToBhv f = bhvUnpack . apply . (cb f &&& bhvPack)



class BhvPrim f a b | f -> a b where
        bhvPrim :: f -> HtmlM (String, [RawJS])





data BhvServer a b = BhvServer String JSString
instance BhvPrim (BhvServer a b) a b where
        bhvPrim (BhvServer func name) = do
                jname <- bhvValue name
                return (func, [jname])


data Time

data Timed a = NotYet | Timed Time a

{-
instance JSON a => JSON (Timed a) where
        readJSON (JSObject x) = case fromJSObject x of
                                     [("NotYet", _)] -> Ok NotYet
                                     [("Data", params)] -> eitherToResult $ do
                                             [jt, jx] <- resultToEither $ readJSON params
                                             t <- resultToEither $ readJSON jt
                                             x <- resultToEither $ readJSON jx
                                             return $ Timed t x

        showJSON NotYet      = JSObject $ toJSObject [("NotYet", JSNull)]
        showJSON (Timed t x) = JSObject $ toJSObject [("Timed", showJSON [showJSON t, showJSON x])]

eitherToResult (Right x) = Ok x
eitherToResult (Left e) = Error e
-}

sget' :: String -> Behaviour (Maybe JSValue)
sget' = Prim . BhvServer "sget" . toJSString

sget :: BJSON a => String -> Behaviour (Maybe a)
sget = b_join . b_fmap (b_result (const b_nothing) b_just . b_readJSON) . sget'




addBehaviour' :: Int -> String -> [RawJS] -> HtmlM ()
addBehaviour' id name params =
        modify $ \s -> s { hsBehaviours = (id, name, params) : hsBehaviours s }


addBehaviourName :: String -> [RawJS] -> HtmlM (Int, Int)
addBehaviourName name params = do
    bid <- htmlUniq
    r <- gets hsRecursion
    addBehaviour' bid name params
    return (r,bid)


addBehaviour :: BehaviourFun a b -> HtmlM (Int,Int)
addBehaviour b = do
        -- We need to do this so b does not need to be evaluated for internal
        -- unique counter and mfix may work for mutually recursive behaviours.
        nid <- htmlUniq
        r <- gets hsRecursion

        case b of
             Prim f -> do
                     (name, params) <- bhvPrim f
                     addBehaviour' nid name params
                     return (r,nid)

             Assigned id -> return id
             BhvID -> return (0,0)


instance BhvValue (BhvFun a b) where
    bhvValue f = do (r,i) <- addBehaviour f
                    return $ RawJS $ "cthunk(r_bhv_fun_"++show r++"["++show i++"])"


data HtmlState = HtmlState
        { hsUniq :: Int
        , hsBehaviours :: [(Int, String, [RawJS])]
        , hsHtmlValues :: [(Int, String, Maybe String)]
        , hsHtmlCurrent :: Maybe Int
        , hsHtmlBehaviours :: [(Int, Maybe Int)]
        , hsHtmlGens :: [(Int, Maybe Int)]
        , hsRecursion :: Int
        }

emptyHtmlState = HtmlState 0 [] [] Nothing [] [] 0

data HtmlStructure = Tag String [Attribute] [HtmlStructure]
                   | Text String
                   | Behaviour Int
                   -- | Placeholder Int [Attribute]

data HtmlM a = HtmlM (HtmlState -> (a, ([HtmlStructure], HtmlState)))

type Html = HtmlM ()


instance Functor HtmlM where
        fmap f (HtmlM hf) = HtmlM $ \s -> (\(x, hs)->(f x, hs)) (hf s)

instance Monad HtmlM where
        return x = HtmlM $ \s -> (x, ([], s))
        (HtmlM f) >>= g = HtmlM $ \s -> let (x, (cs, s')) = f s
                                            (HtmlM g') = g x
                                            (y, (ds, s'')) = g' s'
                                         in (y, (cs++ds, s''))

instance MonadFix HtmlM where
        mfix f = HtmlM $ \s -> let (HtmlM f') = f x
                                   (x, s') = f' s
                                in (x, s')

instance MonadState HtmlState HtmlM where
        get = HtmlM $ \s -> (s, ([], s))
        put s = HtmlM $ \_ -> ((), ([], s))


instance IsString Html where
        fromString text = HtmlM $ \s -> ((), ([Text text], s))
str :: String -> Html
str = fromString


instance Attributable (HtmlM a) where
        (HtmlM f) ! a = HtmlM $ \s -> let (x, (cs, s')) = f s
                                       in (x, (map (addAttr a) cs, s'))

instance Attributable (HtmlM a -> HtmlM a) where
        f ! a = \html -> HtmlM $ \s -> let (HtmlM g) = f html
                                           (x, (t, s')) = g s
                                        in (x, (map (addAttr a) t, s'))

instance Attributable (Behaviour Html) where
        h ! a = binOp "add_attr" h (Prim $ BhvConst a)


addAttr :: Attribute -> HtmlStructure -> HtmlStructure
addAttr a (Tag name as content) = Tag name (a:as) content
addAttr _ t@(Text _) = t
--addAttr a (Behaviour id) = Behaviour $ Prim (AddAttr a) . b
-- addAttr a (Placeholder p as) = Placeholder p (a:as)

data AddAttr = AddAttr Attribute
instance BhvPrim AddAttr Html Html where
        bhvPrim (AddAttr (AttrVal name val)) = do
                jname <- bhvValue name; jval <- bhvValue val
                return ("add_attr", [jname, jval])
        bhvPrim (AddAttr (AttrBool name)) = do
                jname <- bhvValue name; jval <- bhvValue True
                return ("add_attr", [jname, jval])


b_appendHtml :: Bhv Html -> Bhv Html -> Bhv Html
b_appendHtml = binOp "append_html"



jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""
reactive = script ! type_ "text/javascript" ! src "js/reactive.js" $ ""
reactive_prim = script ! type_ "text/javascript" ! src "js/reactive-prim.js" $ ""

initReactive :: Html
initReactive = do
        bs <- gets $ reverse . hsBehaviours
        hs <- gets $ reverse . hsHtmlValues
        hbs <- gets $ reverse . hsHtmlBehaviours
        hgs <- gets $ reverse . hsHtmlGens
        script ! type_ "text/javascript" $ do
                str $ "$(document).ready(function() {\n"

                forM_ bs $ \(i, func, params) ->
                        str $ "r_bhv_fun_0["++show i++"] = new BhvFun("++show i++");\n"

                forM_ hs $ \(i, h, mi) ->
                    str $ "var r_html_"++show i++" = $('"++escapeStringJS h++"')" ++
                    case mi of { Nothing -> ""; Just inner -> ".prop('rawe_html_inner', "++inner++")" }
                    ++ ";\n"

                forM_ hbs $ \(i, mv) ->
                    let var = case mv of Nothing -> "$('body')"
                                         Just v  -> "r_html_"++show v
                     in str $ "r_bhv_fun_0["++show i++"].html = "++var++".find('*[bhv-id="++show i++"]');\n"

                forM_ hgs $ \(i, mv) ->
                    let var = case mv of Nothing -> "$('body')"
                                         Just v  -> "r_html_"++show v
                     in str $ "r_bhv_fun_0["++show i++"].gen = "++var++".find2('*[bhv-gen="++show i++"]');\n"

                forM_ bs $ \(i, func, params) -> do
                        let jid = show i
                            jfunc = "r_prim_"++func
                            jparams = concatMap ((',':).unRawJS) params
                        str $ jfunc++".call(r_bhv_fun_0["++jid++"]"++jparams++");\n"

                str $ "r_init();\n"
                str $ "});\n";




container :: String -> Html -> Html
container tag (HtmlM f) = HtmlM $ \s -> let ((), (content, s')) = f s
                                         in ((), ([Tag tag [] content], s'))

tag :: String -> Html
tag name = HtmlM $ \s -> ((), ([Tag name [] []], s))


head content = container "head" $ do
        jquery
        reactive
        reactive_prim
        content

body content = container "body" $ do
        content
        initReactive


a = container "a"
br = tag "br"
div = container "div"
html = container "html"
li = container "li"
script = container "script"
span = container "span"
title = container "title"
ul = container "ul"

name = AttrVal "name"
type_ = AttrVal "type"
src = AttrVal "src"
style = AttrVal "style"

htmlGen :: String -> ([HtmlStructure] -> HtmlStructure) -> HtmlM b -> HtmlM (Behaviour a)
htmlGen name tag (HtmlM f) = do
    bid@(~(_,i)) <- addBehaviourName ("gen_"++name) []
    cur <- gets hsHtmlCurrent
    HtmlM $ \s -> let (_, (content, s')) = f s
                   in (Assigned bid, ([addAttr (AttrVal "bhv-gen" (show i)) (tag content)],
                       s' { hsHtmlGens = (i, cur) : hsHtmlGens s' } ))

input :: String -> HtmlM (Behaviour a)
input t = htmlGen ("input_"++t) (Tag "input" [AttrVal "type" t]) (return ())



data ToHtmlHtmlList = ToHtmlHtmlList

class ToHtmlBehaviour a where
        toHtml :: Behaviour a -> Behaviour Html

instance ToHtmlBehaviour Html where
        toHtml = id

instance ToHtmlBehaviour Int where
        toHtml = unOp "to_html_int"

instance ToHtmlBehaviour String where
        toHtml = toHtml . b_toJSString

instance ToHtmlBehaviour JSString where
        toHtml = unOp "to_html_jsstring"


instance ToHtmlBehaviour [Html] where
        toHtml = b_foldl b_appendHtml (cb $ div $ return ())

instance ToHtmlBehaviour a => ToHtmlBehaviour (Maybe a) where
        toHtml = b_maybe (cb $ div $ return ()) toHtml


data BhvHtmlB a = BhvHtmlB (Behaviour Html) (Behaviour a)
instance BhvPrim (BhvHtmlB a) () (HtmlM (Behaviour a)) where
        bhvPrim (BhvHtmlB html b) = do
                bid <- addBehaviour b
                htmlv <- bhvValue $ html ! AttrVal "bhv-gen" (show bid)
                return ("const", [htmlv])

toHtmlB :: (ToHtmlBehaviour h) => Behaviour a -> Behaviour h -> Behaviour (HtmlM (Behaviour a))
toHtmlB v x = Prim $ BhvHtmlB (toHtml x) v


b_uncurry :: (Behaviour a -> Behaviour b -> c) -> Behaviour (a, b) -> c
b_uncurry f x = f (fst . x) (snd . x)

b_curry :: (Behaviour (a, b) -> c) -> Behaviour a -> Behaviour b -> c
b_curry f x y = f (x &&& y)

class BhvCurrying a ps r | a -> ps r where
        b_uncurryAll :: a -> Behaviour ps -> r
        b_curryAll :: (Behaviour ps -> r) -> a

instance BhvCurrying (Behaviour a -> Behaviour b) a (Behaviour b) where
        b_uncurryAll = id
        b_curryAll = id

instance (BhvCurrying (Behaviour b -> r) ps r') =>
        BhvCurrying (Behaviour a -> Behaviour b -> r) (a, ps) r' where

        b_uncurryAll f = b_uncurry (\x -> b_uncurryAll (f x))
        b_curryAll f = \x -> b_curryAll $ (b_curry f) x



{- Basic classes and functions -}


class BEq a where
        (~==) :: Behaviour a -> Behaviour a -> Behaviour Bool
        x ~== y = b_not $ x ~/= y
        (~/=) :: Behaviour a -> Behaviour a -> Behaviour Bool
        x ~/= y = b_not $ x ~== y


class BFunctor f where
        b_fmap :: (Behaviour a -> Behaviour b) -> (Behaviour (f a)) -> (Behaviour (f b))


class BFunctor m => BMonad m where
        b_return :: Behaviour a -> Behaviour (m a)

        (~>>=) :: Behaviour (m a) -> (Behaviour a -> Behaviour (m b)) -> Behaviour (m b)
        x ~>>= f = b_join $ b_fmap f x

        b_join :: Behaviour (m (m a)) -> Behaviour (m a)
        b_join = (~>>= id)


b_liftM2 :: (BMonad m) => (Behaviour a -> Behaviour b -> Behaviour c) ->
        Behaviour (m a) -> Behaviour (m b) -> Behaviour (m c)
b_liftM2 f mx my = mx ~>>= \x -> my ~>>= \y -> b_return (f x y)


{- Int instances -}

instance BEq Int where (~==) = binOp "eq"

instance BJSON Int where
        b_readJSON x = b_ite (b_typeof x ~== "number") (b_result_ok $ b_unsafeCoerce x) (b_result_error "readJSON: not a number")
        b_writeJSON = b_unsafeCoerce


{- Char instances -}

instance BEq Char where (~==) = binOp "eq"


{- Bool constructors, destructor, instances and functions -}

b_true :: Behaviour Bool
b_true = primFunc "true"

b_false :: Behaviour Bool
b_false = primFunc "false"

b_bool :: Behaviour a -> Behaviour a -> Behaviour Bool -> Behaviour a
b_bool = terOp "bool"

b_ite c t f = b_bool t f c

instance BEq Bool where
        x ~== y = b_bool y (b_not y) x

b_not :: Behaviour Bool -> Behaviour Bool
b_not = b_bool b_false b_true

(~&&) :: Behaviour Bool -> Behaviour Bool -> Behaviour Bool
x ~&& y = b_bool y x x

(~||) :: Behaviour Bool -> Behaviour Bool -> Behaviour Bool
x ~|| y = b_bool x y x


{- Tuple instances -}

instance BFunctor ((,) a) where
        b_fmap f x = (fst . x) &&& f (snd . x)


{- List constructors, destructor, instances and functions -}

b_nil :: Behaviour [a]
b_nil = primFunc "nil"

(~:) :: Behaviour a -> Behaviour [a] -> Behaviour [a]
(~:) = binOp "cons"
infixr 5 ~:

b_list :: Behaviour b -> (Behaviour a -> Behaviour [a] -> Behaviour b) -> Behaviour [a] -> Behaviour b
b_list n c = terOp "list" n (cb $ haskToBhv $ b_uncurry c)

instance BEq a => BEq [a] where
        (~==) = bfix $ (\z -> \xs ys -> b_list (b_null ys) (\x xs' -> b_list b_false (\y ys' -> (x ~== y) ~&& z xs' ys') ys) xs)

instance BFunctor [] where
        b_fmap = b_map

instance BMonad [] where
        b_return = (~:b_nil)
        b_join = b_concat

(~++) :: Behaviour [a] -> Behaviour [a] -> Behaviour [a]
xs ~++ ys = bfix (\f -> b_list ys (\x xs' -> x ~: f xs')) xs

b_head :: Behaviour [a] -> Behaviour a
b_head = b_list (b_error "Reactive.head") (\x _ -> x)

b_tail :: Behaviour [a] -> Behaviour [a]
b_tail = b_list (b_error "Reactive.tail") (\_ xs -> xs)

b_null :: Behaviour [a] -> Behaviour Bool
b_null = b_list b_true (\_ _ -> b_false)

b_length :: Behaviour [b] -> Behaviour Int
b_length = bfix (\f -> b_list 0 (\_ xs -> 1 + f xs))

b_map :: (Behaviour a -> Behaviour b) -> Behaviour [a] -> Behaviour [b]
b_map f = bfix (\m -> b_list b_nil (\x xs -> f x ~: m xs))

b_foldl :: (Behaviour a -> Behaviour b -> Behaviour a) -> Behaviour a -> Behaviour [b] -> Behaviour a
b_foldl f = bfix (\fld -> \z -> b_list z (\x xs -> fld (f z x) xs))

b_foldr :: (Behaviour a -> Behaviour b -> Behaviour b) -> Behaviour b -> Behaviour [a] -> Behaviour b
b_foldr f z = bfix (\fld -> b_list z (\x xs -> x `f` fld xs))

b_concat :: Behaviour [[a]] -> Behaviour [a]
b_concat = b_foldr (~++) b_nil

b_and :: Behaviour [Bool] -> Behaviour Bool
b_and = b_foldr (~&&) b_true

b_or :: Behaviour [Bool] -> Behaviour Bool
b_or = b_foldr (~||) b_false

b_all :: (Behaviour a -> Behaviour Bool) -> Behaviour [a] -> Behaviour Bool
b_all f = b_and . b_map f

b_lookup :: (BEq a) => Behaviour a -> Behaviour [(a,b)] -> Behaviour (Maybe b)
b_lookup k = bfix $ \f -> b_list b_nothing $ \x xs -> flip b_uncurry x $ \k' v -> b_ite (k'~==k) (b_just v) (f xs)

b_zip :: Behaviour [a] -> Behaviour [b] -> Behaviour [(a, b)]
b_zip = b_zipWith (&&&)

b_zipWith :: (Behaviour a -> Behaviour b -> Behaviour c) -> Behaviour [a] -> Behaviour [b] -> Behaviour [c]
b_zipWith f = bfix $ (\z -> \xs ys -> b_list b_nil (\x xs' -> b_list b_nil (\y ys' -> f x y ~: z xs' ys') ys) xs)


{- Maybe constructors, destructor, instances and functions -}

b_nothing :: Behaviour (Maybe a)
b_nothing = primFunc "nothing"

b_just :: Behaviour a -> Behaviour (Maybe a)
b_just = unOp "just"

b_maybe :: Behaviour b -> (Behaviour a -> Behaviour b) -> Behaviour (Maybe a) -> Behaviour b
b_maybe def f = terOp "maybe" def (cb $ haskToBhv f)

instance BEq a => BEq (Maybe a) where
        x ~== y = b_maybe (b_maybe b_true (const b_false) y) (\xv -> b_maybe b_false (xv ~==) y) x

instance BFunctor Maybe where
        b_fmap f = b_maybe b_nothing (b_just . f)

instance BMonad Maybe where
        b_return = b_just
        x ~>>= f = b_maybe b_nothing f x
        b_join = b_maybe b_nothing id

b_fromJust :: Behaviour (Maybe b) -> Behaviour b
b_fromJust = b_maybe (b_error "fromJust: Nothing") id



{- Timed constructors, destructor and instances -}

b_notYet :: Behaviour (Timed a)
b_notYet = primFunc "not_yet"

b_onTime :: Bhv Time -> Bhv a -> Bhv (Timed a)
b_onTime = binOp "on_time"

b_timed :: Bhv b -> (Bhv Time -> Bhv a -> Bhv b) -> Bhv (Timed a) -> Bhv b
b_timed def f = terOp "timed" def (cb $ haskToBhv $ b_uncurry f)

data TimedFold a b = TimedFold (Bhv Time -> Bhv a -> Bhv b -> Bhv a) (Bhv a) (Bhv (Timed b))
instance BhvPrim (TimedFold a b) () a where
    bhvPrim (TimedFold step def ev) = do
        jstep <- bhvValue $ haskToBhv $ b_uncurryAll step
        jdef <- bhvValue def
        jev <- bhvValue ev
        return ("timed_fold", [jstep, jdef, jev])

b_timedFold :: (Bhv Time -> Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv (Timed b) -> Bhv a
b_timedFold f x = Prim . TimedFold f x


instance BFunctor Timed where
    b_fmap f = b_timed b_notYet (\t x -> b_onTime t (f x))


{- Result constructors and destructor -}

b_result_ok :: Behaviour a -> Behaviour (Result a)
b_result_ok = unOp "result_ok"

b_result_error :: Behaviour String -> Behaviour (Result a)
b_result_error = unOp "result_error"

b_result :: (Behaviour String -> Behaviour b) -> (Behaviour a -> Behaviour b) -> Behaviour (Result a) -> Behaviour b
b_result a b = terOp "result" (cb $ haskToBhv a) (cb $ haskToBhv b)


{- JSON types, functions and instances -}

class BJSON a where
        b_readJSON :: Behaviour JSValue -> Behaviour (Result a)
        b_writeJSON :: Behaviour a -> Behaviour JSValue


b_toJSString :: Behaviour String -> Behaviour JSString
b_toJSString = unOp "to_js_string"

b_fromJSString :: Behaviour JSString -> Behaviour String
b_fromJSString = unOp "from_js_string"

instance BEq JSString where
        (~==) = binOp "eq"


b_toJSObject' :: Behaviour [(JSString, a)] -> Behaviour (JSObject a)
b_toJSObject' = unOp "to_js_object"

b_toJSObject :: Behaviour [(String, a)] -> Behaviour (JSObject a)
b_toJSObject = b_toJSObject' . b_map (\x -> b_toJSString (fst . x) &&& (snd . x))

b_fromJSObject' :: Behaviour (JSObject a) -> Behaviour [(JSString, a)]
b_fromJSObject' = unOp "from_js_object"

b_fromJSObject :: Behaviour (JSObject a) -> Behaviour [(String, a)]
b_fromJSObject = b_map (\x -> b_fromJSString (fst . x) &&& (snd . x)) . b_fromJSObject'

instance BFunctor JSObject where
        b_fmap f = binOp "js_object_fmap" (cb $ haskToBhv f)


{- Other functions -}

b_unsafeCoerce :: Behaviour a -> Behaviour b
b_unsafeCoerce = (.) (Assigned (0,0))

b_typeof' :: Behaviour JSValue -> Behaviour JSString
b_typeof' = unOp "typeof"

b_typeof :: Behaviour JSValue -> Behaviour String
b_typeof = b_fromJSString . b_typeof'




data BhvEnumFromTo = BhvEnumFromTo
instance BhvPrim BhvEnumFromTo (Int,Int) [Int] where
        bhvPrim BhvEnumFromTo = return ("enum_from_to", [])
benumFromTo :: BehaviourFun (Int,Int) [Int]
benumFromTo = Prim BhvEnumFromTo

data BhvConst a b = (BhvValue b) => BhvConst b
instance BhvPrim (BhvConst a b) a b where
        bhvPrim (BhvConst x) = do jx <- bhvValue x
                                  return ("const", [jx])


data BhvPrimFunc a b = BhvPrimFunc String
instance BhvPrim (BhvPrimFunc a b) a b where
        bhvPrim (BhvPrimFunc name) = return (name, [])

data BhvModifier a b c d = BhvModifier String (BehaviourFun a b)
instance BhvPrim (BhvModifier a b c d) c d where
        bhvPrim (BhvModifier name x) = do jx <- bhvValue x
                                          return (name, [jx])

data BhvModifier2 a b c d e f = BhvModifier2 String (BehaviourFun a b) (BehaviourFun c d)
instance BhvPrim (BhvModifier2 a b c d e f) e f where
        bhvPrim (BhvModifier2 name x y) = do jx <- bhvValue x; jy <- bhvValue y
                                             return (name, [jx, jy])


bcurry :: BehaviourFun (a, b) c -> BehaviourFun d a -> BehaviourFun d b -> BehaviourFun d c
bcurry f x y = f . (x &&& y)

primFunc :: String -> BehaviourFun a b
primFunc name = Prim (BhvPrimFunc name)
unOp :: String -> BehaviourFun a b -> BehaviourFun a c

unOp  name x     = primFunc name .  x
binOp name x y   = primFunc name . (x &&& y)
terOp name x y z = primFunc name . (x &&& y &&& z)

bjoin :: Behaviour (BehaviourFun a b) -> BehaviourFun a b
bjoin = Prim . BhvModifier "bjoin"

b_fix :: (Behaviour a -> Behaviour a) -> Behaviour a
b_fix = unOp "fix" . cb . haskToBhv


class BhvFix a where
        bfix :: (a -> a) -> a

instance BhvFix (Behaviour a) where
        bfix = b_fix

instance (BhvFix b, BhvCurrying (Behaviour a -> b) ps (Behaviour d)) => BhvFix (Behaviour a -> b) where
        bfix f = b_curryAll $ (.) $ bjoin $ b_fix ((\x -> cb $ haskToBhv $ f' x) . (.) . bjoin)
                where f' = b_uncurryAll . \f1 -> f (b_curryAll f1)


instance Eq (BehaviourFun a b) where
        _ == _ = error "Eq instance for behaviours is required for Num, but is meaningless"
instance Show (BehaviourFun a b) where
        show _ = error "Show instance for behaviours is required for Num, but is meaningless"
instance Num (BehaviourFun a Int) where
        fromInteger = Prim . BhvConst . fromInteger
        (+) = binOp "plus"
        (*) = binOp "times"
        abs = unOp "abs"
        signum = unOp "signum"

instance Num (BehaviourFun a (Maybe Int)) where
        fromInteger = Prim . BhvConst . Just . fromInteger
        (+) = binOp "plus_mb"
        (*) = binOp "times_mb"
        abs = unOp "abs_mb"
        signum = unOp "signum_mb"



class BehaviourToHtml a where
        bhv :: Behaviour (HtmlM a) -> HtmlM a

instance BehaviourToHtml () where
    bhv x = do ~(_,id) <- addBehaviour x
               cur <- gets hsHtmlCurrent
               HtmlM $ \s -> ((), ([Behaviour id], s { hsHtmlBehaviours = (id, cur) : hsHtmlBehaviours s } ))

instance BehaviourToHtml (Behaviour a) where
    bhv x = do ~(r,id) <- addBehaviour x
               cur <- gets hsHtmlCurrent
               jbhv <- bhvValue (Assigned (r,id) :: Behaviour (HtmlM (Behaviour a)))
               nid <- addBehaviourName "bhv_to_html_inner" [jbhv]
               HtmlM $ \s -> (Assigned nid, ([Behaviour id], s { hsHtmlBehaviours = (id, cur) : hsHtmlBehaviours s } ))



cb :: (BhvValue a) => a -> BehaviourFun b a
cb x = Prim $ BhvConst x



newBhv :: HtmlM (Behaviour a)
newBhv = undefined

(<--) :: Behaviour a -> HtmlM (Behaviour a) -> Html
bhv <-- html = undefined


post' :: String -> Behaviour (Timed (JSObject JSString)) -> HtmlM (Behaviour (Maybe JSValue))
post' name x = fmap Assigned $ addBehaviour $ (Prim $ BhvServer "spost" $ toJSString name) . x

post :: (BJSON a) => String -> Behaviour (Timed [(String,String)]) -> HtmlM (Behaviour (Maybe a))
post name = fmap (b_join . b_fmap (b_result (const b_nothing) b_just . b_readJSON)) .
    post' name . b_fmap (b_toJSObject . b_fmap (b_fmap b_toJSString))

b_debug :: Behaviour String -> Behaviour a -> Behaviour a
b_debug = binOp "debug"


b_until :: Behaviour (HtmlM (Behaviour a)) -> Behaviour (Maybe Html) -> Behaviour (HtmlM (Behaviour a))
b_until = binOp "until"

{-
b_until :: Behaviour a -> Behaviour (Maybe a) -> Behaviour a
b_until def = b_maybe def id
-}


instance IsString (Behaviour Html) where
        fromString = cb . span . fromString

instance IsString (Behaviour String) where
        fromString = cb


b_error' :: Behaviour JSString -> Behaviour a
b_error' = unOp "error"

b_error :: Behaviour String -> Behaviour a
b_error = b_error' . b_toJSString

b_undefined = b_error "undefined"

b_guardTimed :: (Behaviour a -> Behaviour Bool) -> Behaviour (Timed a) -> Behaviour (Timed a)
b_guardTimed f tx = b_ite (b_timed b_false (const f) tx) tx b_notYet


form' :: HtmlM a -> HtmlM (Behaviour (Timed (JSObject JSString)))
form' = htmlGen "form" (Tag "form" [])

form :: HtmlM a -> HtmlM (Behaviour (Timed [(String, String)]))
form = fmap (b_fmap (b_fromJSObject . b_fmap b_fromJSString)) . form'

t2m :: Behaviour (Timed a) -> Behaviour (Maybe a)
t2m = b_timed b_nothing (const b_just)

textfield' :: String -> HtmlM (Behaviour JSString)
textfield' n = input "text" ! name n

textfield :: String -> HtmlM (Behaviour String)
textfield = fmap b_fromJSString . textfield'

submit' :: HtmlM (Behaviour (Timed JSString))
submit' = input "submit"

submit :: HtmlM (Behaviour (Timed String))
submit = fmap (b_fmap b_fromJSString) $ submit'

button :: HtmlM (Bhv (Timed ()))
button = input "button"

(~<) :: BehaviourFun a Int -> BehaviourFun a Int -> BehaviourFun a Bool
(~<) = binOp "lt_int"

x ~>= y = b_not (x ~< y)



data RenderState = RenderState { rsUniq :: Int, rsJavascript :: String }
type RenderMonad a = Writer String a


render :: Html -> String
render html = let (HtmlM f) = renderH html
                  ((result, ()), _) = f (emptyHtmlState { hsUniq = 1, hsBehaviours = [(0, "id", [])] } )
               in result
--render (HtmlM xs) = snd $ runWriter $ evalStateT (mapM render' $ fst $ snd $ xs $ HtmlState 1 []) (RenderState 1 "")


renderH :: HtmlM a -> HtmlM (String, a)
renderH (HtmlM f) = do
        (x, (xs, s')) <- gets f
        put s'
        return (execWriter (mapM_ render' xs), x)

        where render' :: HtmlStructure -> RenderMonad ()

              render' (Tag tag attrs xs) = do
                      tell $ "<" ++ tag
                      mapM_ renderAttrs attrs
                      tell ">\n"
                      mapM render' xs
                      tell $ "</" ++ tag ++ ">\n"

              render' (Text text) = tell text

              render' (Behaviour id) = do
                      tell $ "<div bhv-id="++show id++"></div>\n"

{-
              render' (Placeholder p attrs) = do
                      tell $ "<div placeholder-id=\""++show p++"\""
                      mapM_ renderAttrs attrs
                      tell "></div>\n"
                      -}

              renderAttrs (AttrBool name) = tell $ ' ':name
              renderAttrs (AttrVal name val) = tell $ " "++name++"=\""++escapeStringHtml val++"\""

              renderJavascript = do
                      tell "<script type=\"text/javascript\">\n"
                      tell =<< gets rsJavascript
                      tell "</script>\n"
                      modify $ \s -> s { rsJavascript = [] }



htmlUniq :: HtmlM Int
htmlUniq = do { x <- gets hsUniq; modify $ \s -> s { hsUniq = x+1 }; return x }


htmlLocal :: HtmlM a -> HtmlM a
htmlLocal action = do
    state <- get
    put $ emptyHtmlState { hsRecursion = hsRecursion state + 1 }
    result <- action
    put state
    return result
