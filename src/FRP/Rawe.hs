{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs,
  FunctionalDependencies, OverloadedStrings,
  TypeSynonymInstances, NoMonomorphismRestriction #-}

{-# LANGUAGE DoRec #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}


module FRP.Rawe where


import Prelude hiding (head,div,(.),id,fst,snd,span)

import Control.Categorical.Bifunctor
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Category.Monoidal

import Control.Monad.State
import Control.Monad.Writer hiding (Product)

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

type Bhv a = BehaviourFun Void a

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


instance PFunctor (,) BhvFun BhvFun where
    first = firstDefault

instance QFunctor (,) BhvFun BhvFun where
    second = secondDefault

instance Bifunctor (,) BhvFun BhvFun BhvFun where
    bimap = bimapProduct

instance Braided BhvFun (,) where
    braid = braidProduct

instance Symmetric BhvFun (,)

instance Associative BhvFun (,) where
    associate = associateProduct

instance Disassociative BhvFun (,) where
    disassociate = disassociateProduct

type instance Id BhvFun (,) = Void

instance Monoidal BhvFun (,) where
    idl = snd
    idr = fst

instance PreCartesian BhvFun where
    type Product BhvFun = (,)
    x &&& y = Prim $ BhvModifier2 "product" x y
    fst = primOp "fst"
    snd = primOp "snd"

instance CCC BhvFun where
    type Exp BhvFun = (->)
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

data Timed a = NotYet | OnTime Time a

{-
instance JSON a => JSON (Timed a) where
        readJSON (JSObject x) = case fromJSObject x of
                                     [("NotYet", _)] -> Ok NotYet
                                     [("Data", params)] -> eitherToResult $ do
                                             [jt, jx] <- resultToEither $ readJSON params
                                             t <- resultToEither $ readJSON jt
                                             x <- resultToEither $ readJSON jx
                                             return $ OnTime t x

        showJSON NotYet       = JSObject $ toJSObject [("NotYet", JSNull)]
        showJSON (OnTime t x) = JSObject $ toJSObject [("OnTime", showJSON [showJSON t, showJSON x])]

eitherToResult (Right x) = Ok x
eitherToResult (Left e) = Error e
-}




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

script = container "script"
span = container "span"

type_ = AttrVal "type"
src = AttrVal "src"





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

{-
b_liftM2 :: (BMonad m) => (Behaviour a -> Behaviour b -> Behaviour c) ->
        Behaviour (m a) -> Behaviour (m b) -> Behaviour (m c)
b_liftM2 f mx my = mx ~>>= \x -> my ~>>= \y -> b_return (f x y)
-}



{- Timed constructors, destructor and instances -}

notYet :: Behaviour (Timed a)
notYet = primFunc "not_yet"

onTime :: Bhv Time -> Bhv a -> Bhv (Timed a)
onTime = binOp "on_time"

timed :: Bhv b -> (Bhv Time -> Bhv a -> Bhv b) -> Bhv (Timed a) -> Bhv b
timed def f = terOp "timed" def (cb $ haskToBhv $ b_uncurry f)

data TimedFold a b = TimedFold (Bhv Time -> Bhv a -> Bhv b -> Bhv a) (Bhv a) (Bhv (Timed b))
instance BhvPrim (TimedFold a b) Void a where
    bhvPrim (TimedFold step def ev) = do
        jstep <- bhvValue $ haskToBhv $ b_uncurryAll step
        jdef <- bhvValue def
        jev <- bhvValue ev
        return ("timed_fold", [jstep, jdef, jev])

timedFold :: (Bhv Time -> Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv (Timed b) -> Bhv a
timedFold f x = Prim . TimedFold f x




b_toJSString :: Behaviour String -> Behaviour JSString
b_toJSString = unOp "to_js_string"

b_fromJSString :: Behaviour JSString -> Behaviour String
b_fromJSString = unOp "from_js_string"


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

primOp :: String -> BhvFun a b
primOp = Prim . BhvPrimFunc

primOp1 name a         = primOp name . (cbf a)
primOp2 name a b       = primOp name . (cbf a &&& cbf b)
primOp3 name a b c     = primOp name . (cbf a &&& cbf b &&& cbf c)
primOp4 name a b c d   = primOp name . (cbf a &&& cbf b &&& cbf c &&& cbf d)
primOp5 name a b c d e = primOp name . (cbf a &&& cbf b &&& cbf c &&& cbf d &&& cbf e)

bjoin :: Behaviour (BehaviourFun a b) -> BehaviourFun a b
bjoin = Prim . BhvModifier "bjoin"

b_fix :: forall a. (Behaviour a -> Behaviour a) -> Behaviour a
b_fix = primOp1 "fix"


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
