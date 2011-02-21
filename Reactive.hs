{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs,
  FunctionalDependencies, OverloadedStrings,
  TypeSynonymInstances, NoMonomorphismRestriction #-}

{-# LANGUAGE DoRec #-}
{-# LANGUAGE EmptyDataDecls #-}

module Reactive where

import Prelude hiding (head,div,(.),id,fst,snd,span)

import Control.Category
import Control.Category.Cartesian
import Control.Functor

import Control.Monad.State
import Control.Monad.Writer

import Data.String

import Text.JSON hiding (JSValue)


import Debug.Trace


data Attribute = AttrVal String String
               | AttrBool String

class Attributable b where
        (!) :: b -> Attribute -> b


newtype RawJS = RawJS { unRawJS :: String }
instance IsString RawJS where fromString = RawJS



class JSValue a where
        jsValue :: a -> HtmlM RawJS
        jsValueList :: [a] -> HtmlM RawJS
        jsValueList = return.RawJS.("cthunk(["++).(++"])") . drop 1 . concat <=<
                        mapM (return.(',':).(\(RawJS x)->x) <=< jsValue)

instance (JSValue a) => JSValue [a] where
        jsValue = jsValueList

instance JSValue () where jsValue () = return "cthunk([])"
instance JSValue Int where jsValue x = return.RawJS $ "cthunk("++show x++")"
instance JSValue Bool where jsValue x = return $ if x then "cthunk(true)" else "cthunk(false)"

instance JSValue Attribute where
        jsValue (AttrVal name value) = do
                (RawJS jn) <- jsValue name; (RawJS jv) <- jsValue value
                return.RawJS $ "cthunk( { AttrVal: ["++jn++","++jv++"] } )"
        jsValue (AttrBool name) = do
                (RawJS jn) <- jsValue name
                return.RawJS $ "cthunk( { AttrBool: ["++jn++"] } )"


instance JSValue (HtmlM a) where
        jsValue html = do
                (raw, _) <- renderH html
                return.RawJS $ "cthunk( $('"++escapeStringJS raw++"') )"

jsHtml :: HtmlM a -> HtmlM (RawJS, a)
jsHtml html = do
        (raw, x) <- renderH html
        return (RawJS $ "$('"++escapeStringJS raw++"')", x)

instance JSValue Char where
        jsValue x = return.RawJS $ "cthunk("++show x++")"
        jsValueList x = return.RawJS $ "cthunk("++show x++")"

instance (JSValue a, JSValue b) => JSValue (a, b) where
        jsValue (x,y) = do
                (RawJS x') <- jsValue x; (RawJS y') <- jsValue y
                return.RawJS $ "cthunk(["++x'++", "++y'++"])"

instance (JSValue a) => JSValue (Maybe a) where
        jsValue (Just x) = do (RawJS jx) <- jsValue x
                              return.RawJS $ "cthunk({Just:"++jx++"})"
        jsValue Nothing  = return.RawJS $ "cthunk({Nothing:null})"

{-
instance (JSValue a) => JSValue (Timed a) where
        jsValue (Timed t x) = do (RawJS jt) <- jsValue t
                                 (RawJS jx) <- jsValue x
                                 return.RawJS $ "{Timed:["++jt++","++jx++"]}"
        jsValue NotYet = return.RawJS $ "{NotYet:null}"
        -}


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


data BehaviourFun a b = forall f. (BehaviourPrim f a b) => Prim f
                      | Assigned Int
                      | (a ~ b) => BhvID


type Behaviour a = BehaviourFun () a




instance Category BehaviourFun where
        id = BhvID
        BhvID . b = b
        b . BhvID = b
        x@(Prim _) . y@(Prim _) = Prim $ BhvModifier2 "compose" y x
        x@(Prim _) . y@(Assigned _) = Prim $ BhvModifier2 "compose" y x
        x@(Assigned _) . y@(Prim _) = Prim $ BhvModifier2 "compose" y x
        x@(Assigned _) . y@(Assigned _) = Prim $ BhvModifier2 "compose" y x


instance PFunctor (,) BehaviourFun BehaviourFun where
        first = Prim . BhvModifier "first"

instance QFunctor (,) BehaviourFun BehaviourFun where
        second = Prim . BhvModifier "second"

instance Bifunctor (,) BehaviourFun BehaviourFun BehaviourFun where
        bimap x y = Prim $ BhvModifier2 "bimap" x y

instance Braided BehaviourFun (,) where
        braid = primFunc "braid"

instance Associative BehaviourFun (,) where
        associate = primFunc "associate"

instance Coassociative BehaviourFun (,) where
        coassociate = primFunc "coassociate"

instance PreCartesian BehaviourFun (,) where
        x &&& y = Prim $ BhvModifier2 "product" x y
        fst = primFunc "fst"
        snd = primFunc "snd"




class BehaviourPrim f a b | f -> a b where
        bhvPrim :: f -> HtmlM (String, [RawJS])





data BhvServer a b = BhvServer String String
instance BehaviourPrim (BhvServer a b) a b where
        bhvPrim (BhvServer func name) = do
                jname <- jsValue name
                return (func, [jname])


data Timed a -- = NotYet | Timed Int a

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

sget :: (JSON a, JSON b) => String -> BehaviourFun a (Maybe b)
sget = Prim . BhvServer "sget"

spost :: (JSON a) => String -> Behaviour (Timed [(String, String)]) -> Behaviour (Maybe a)
spost name = (.) (Prim $ BhvServer "spost" name)





addBehaviour' :: Int -> String -> [RawJS] -> HtmlM ()
addBehaviour' id name params =
        modify $ \s -> s { hsBehaviours = (id, name, params) : hsBehaviours s }


addBehaviour :: BehaviourFun a b -> HtmlM Int
addBehaviour b = do
        -- We need to do this so b does not need to be evaluated for internal
        -- unique counter and mfix may work for mutually recursive behaviours.
        nid <- htmlUniq

        case b of
             Prim f -> do
                     (name, params) <- bhvPrim f
                     addBehaviour' nid name params
                     return nid

             Assigned id -> return id
             BhvID -> return (-1)


instance JSValue (BehaviourFun a b) where
        jsValue f = do jid <- return.show =<< addBehaviour f
                       return $ RawJS $ "cthunk(r_behaviours["++jid++"])"


data HtmlState = HtmlState
        { hsUniq :: Int
        , hsBehaviours :: [(Int, String, [RawJS])]
        , hsHtmlBehaviours :: [Int]
        }

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
instance BehaviourPrim AddAttr Html Html where
        bhvPrim (AddAttr (AttrVal name val)) = do
                jname <- jsValue name; jval <- jsValue val
                return ("add_attr", [jname, jval])
        bhvPrim (AddAttr (AttrBool name)) = do
                jname <- jsValue name; jval <- jsValue True
                return ("add_attr", [jname, jval])



jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""
reactive = script ! type_ "text/javascript" ! src "js/reactive.js" $ ""
reactive_prim = script ! type_ "text/javascript" ! src "js/reactive-prim.js" $ ""

initReactive :: Html
initReactive = do
        bs <- gets $ reverse . hsBehaviours
        hbs <- gets $ reverse . hsHtmlBehaviours
        script ! type_ "text/javascript" $ do
                str $ "$(document).ready(function() {\n"
                forM_ bs $ \(id, func, params) ->
                        str $ "r_behaviours["++show id++"] = new Behaviour("++show id++");\n"
                forM_ hbs $ \id ->
                        str $ "r_behaviours["++show id++"].html = true;\n"
                forM_ bs $ \(id, func, params) -> do
                        let jid = show id
                            jfunc = "r_prim_"++func
                            jparams = concatMap ((',':).unRawJS) params
                        str $ jfunc++".call(r_behaviours["++jid++"]"++jparams++");\n"
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


input :: HtmlM (Behaviour String)
input = do
        bid <- htmlUniq
        addBehaviour' bid "gen" []
        HtmlM $ \s -> (Assigned bid, ([Tag "input" [AttrVal "bhv-gen" (show bid)] []], s))



data ToHtmlHtmlList = ToHtmlHtmlList

class ToHtmlBehaviour a where
        toHtml :: Behaviour a -> Behaviour Html

instance ToHtmlBehaviour Html where
        toHtml = id

instance ToHtmlBehaviour Int where
        toHtml = unOp "to_html_int"

instance ToHtmlBehaviour String where
        toHtml = unOp "to_html_string"

instance ToHtmlBehaviour a => ToHtmlBehaviour (Maybe a) where
        toHtml = b_maybe (cb $ div $ return ()) toHtml


data BhvHtmlB a = BhvHtmlB (Behaviour Html) (Behaviour a)
instance BehaviourPrim (BhvHtmlB a) () (HtmlM (Behaviour a)) where
        bhvPrim (BhvHtmlB html b) = do
                bid <- addBehaviour b
                htmlv <- jsValue $ html ! AttrVal "bhv-gen" (show bid)
                return ("const", [htmlv])

toHtmlB :: (ToHtmlBehaviour h) => Behaviour a -> Behaviour h -> Behaviour (HtmlM (Behaviour a))
toHtmlB v x = Prim $ BhvHtmlB (toHtml x) v


data HaskToBhv a b = HaskToBhv (Behaviour a -> Behaviour b)
instance BehaviourPrim (HaskToBhv a b) a b where
        bhvPrim (HaskToBhv f) = do iid <- htmlUniq; ji <- jsValue $ Assigned iid
                                   addBehaviour' iid "hask_to_bhv_inner" []
                                   jf <- jsValue (f $ Assigned iid)
                                   return ("hask_to_bhv", [ji, jf])

haskToBhv :: (Behaviour a -> Behaviour b) -> BehaviourFun a b
haskToBhv = Prim . HaskToBhv


{- Bool constructors and destructor #-}

b_true :: Behaviour Bool
b_true = primFunc "true"

b_false :: Behaviour Bool
b_false = primFunc "false"

b_bool :: Behaviour a -> Behaviour a -> Behaviour Bool -> Behaviour a
b_bool = terOp "bool"

b_ite c t f = b_bool t f c


{- Maybe constructors and destructor #-}

b_nothing :: Behaviour (Maybe a)
b_nothing = primFunc "nothing"

b_just :: Behaviour a -> Behaviour (Maybe a)
b_just = unOp "just"

b_maybe :: Behaviour b -> (Behaviour a -> Behaviour b) -> Behaviour (Maybe a) -> Behaviour b
b_maybe def f = terOp "maybe" def (cb $ haskToBhv f)


{- Timed constructors and destructor #-}

b_notYet :: Behaviour (Timed a)
b_notYet = primFunc "not_yet"

{-
b_onTime :: Behaviour a -> Behaviour (Timed a)
b_onTime = unOp "on_time"
-}

b_timed :: Behaviour b -> (Behaviour a -> Behaviour b) -> Behaviour (Timed a) -> Behaviour b
b_timed def f = terOp "timed" def (cb $ haskToBhv f)



data BhvEnumFromTo = BhvEnumFromTo
instance BehaviourPrim BhvEnumFromTo (Int,Int) [Int] where
        bhvPrim BhvEnumFromTo = return ("enum_from_to", [])
benumFromTo :: BehaviourFun (Int,Int) [Int]
benumFromTo = Prim BhvEnumFromTo

data BhvConst a b = (JSValue b) => BhvConst b
instance BehaviourPrim (BhvConst a b) a b where
        bhvPrim (BhvConst x) = do jx <- jsValue x
                                  return ("const", [jx])


data BhvPrimFunc a b = BhvPrimFunc String
instance BehaviourPrim (BhvPrimFunc a b) a b where
        bhvPrim (BhvPrimFunc name) = return (name, [])

data BhvModifier a b c d = BhvModifier String (BehaviourFun a b)
instance BehaviourPrim (BhvModifier a b c d) c d where
        bhvPrim (BhvModifier name x) = do jx <- jsValue x
                                          return (name, [jx])

data BhvModifier2 a b c d e f = BhvModifier2 String (BehaviourFun a b) (BehaviourFun c d)
instance BehaviourPrim (BhvModifier2 a b c d e f) e f where
        bhvPrim (BhvModifier2 name x y) = do jx <- jsValue x; jy <- jsValue y
                                             return (name, [jx, jy])


bcurry :: BehaviourFun (a, b) c -> BehaviourFun d a -> BehaviourFun d b -> BehaviourFun d c
bcurry f x y = f . (x &&& y)

primFunc :: String -> BehaviourFun a b
primFunc name = Prim (BhvPrimFunc name)
unOp :: String -> BehaviourFun a b -> BehaviourFun a c

unOp  name x     = primFunc name .  x
binOp name x y   = primFunc name . (x &&& y)
terOp name x y z = primFunc name . (x &&& y &&& z)


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
        bhv x = do id <- addBehaviour x
                   HtmlM $ \s -> ((), ([Behaviour id], s { hsHtmlBehaviours = id : hsHtmlBehaviours s } ))

instance BehaviourToHtml (Behaviour a) where
        bhv x = do id <- addBehaviour x
                   jbhv <- jsValue (Assigned id :: Behaviour (HtmlM (Behaviour a)))
                   nid <- htmlUniq
                   addBehaviour' nid "bhv_to_html_inner" [jbhv]
                   HtmlM $ \s -> (Assigned nid, ([Behaviour id], s { hsHtmlBehaviours = id : hsHtmlBehaviours s } ))



cb :: (JSValue a) => a -> BehaviourFun b a
cb x = Prim $ BhvConst x

page = html $ do
        head $ do
                title "Catopsis"
        body $ do
                {-
                let count = srvval "count" :: Behaviour Int
                ul $ do
                        let msg = srvval "msg" :: Behaviour String
                        let msgs = srvval "msgs" :: Behaviour [String]

                        li $ "polozka"
                        bhv $ Prim li . Prim ToHtmlInt . count
                        bhv $ Prim li . Prim ToHtmlString . msg
                        bhv $ Prim ToHtmlHtmlList . bmap li . bmap ToHtmlString . msgs
                        bhv $ Prim li . Prim ToHtmlInt . blength . msgs
                        bhv $ Prim li . Prim ToHtmlString . Prim (BhvConst "aoeu")
                        return ()

                bhv $ Prim ToHtmlHtmlList . bmap (div ! style "display: inline; color: blue") . bmap ToHtmlInt . benumFromTo . (1 &&& count)
                -}

                let count = sget "count" :: Behaviour (Maybe Int)
                let register = spost "register" :: Behaviour (Timed [(String,String)]) -> Behaviour (Maybe Int)

                bhv $ bstr "Prvni"
                br
                bhv $ toHtml $ count + 2
                br

                rec result <- post "register" $ b_guardTimed checkForm $ formData
                                :: HtmlM (Behaviour (Maybe Int))
                    formData <- bhv $ (
                        cb $ form $ do
                                bhv $ bstr "Druhy"
                                br
                                name <- textfield "name"
                                br
                                textfield "pass" >> br
                                bhv $ bstr "Heslo je prilis kratke" `displayIfNot`
                                        b_timed b_true checkPassLen formData
                                br
                                textfield "pass-check" >> br
                                bhv $ bstr "Hesla se neshoduji" `displayIfNot`
                                        b_timed b_true checkPassMatch formData
                                br
                                bhv $ toHtml . b_join . b_fmap (b_lookup "name") $ t2m formData
                                submit
                                bhv $ toHtml name
                        ) `b_until` (
                        b_timed b_nothing (const (b_just $ bstr "Odesilani...")) $ b_guardTimed checkForm formData
                        ) `b_until` (
                        b_fmap toHtml result
                        )

                return ()

post :: (JSON a, JSON b) => String -> Behaviour (Timed a) -> HtmlM (Behaviour (Maybe b))
post name x = do id <- addBehaviour $ (Prim $ BhvServer "spost" name) . x
                 HtmlM $ \s -> (Assigned id, ([], s { hsHtmlBehaviours = id : hsHtmlBehaviours s } ))

bstr :: String -> Behaviour Html
bstr = fromString

b_debug :: Behaviour String -> Behaviour a -> Behaviour a
b_debug = binOp "debug"

getVal name = b_fromJust . b_lookup name

checkPassLen :: Behaviour [(String, String)] -> Behaviour Bool
checkPassLen x = (b_length $ getVal "pass" x) ~>= 2

checkPassMatch :: Behaviour [(String, String)] -> Behaviour Bool
checkPassMatch x = getVal "pass" x ~== getVal "pass-check" x

checkForm x = checkPassLen x ~&& checkPassMatch x

displayIf :: (ToHtmlBehaviour a) => Behaviour a -> Behaviour Bool -> Behaviour Html
displayIf what = toHtml . b_bool (b_just what) b_nothing
displayIfNot what = displayIf what . b_not


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


b_error :: Behaviour String -> Behaviour a
b_error = unOp "error"

b_undefined = b_error "undefined"

b_guardTimed :: (Behaviour a -> Behaviour Bool) -> Behaviour (Timed a) -> Behaviour (Timed a)
b_guardTimed f tx = b_ite (b_timed b_false f tx) tx b_notYet


form :: HtmlM a -> HtmlM (Behaviour (Timed [(String, String)]))
form (HtmlM f) = do
        bid <- htmlUniq
        addBehaviour' bid "gen" []
        HtmlM $ \s -> let (_, (content, s')) = f s
                       in (Assigned bid, ([Tag "form" [AttrVal "bhv-gen" (show bid)] content], s'))

t2m :: Behaviour (Timed a) -> Behaviour (Maybe a)
t2m = b_timed b_nothing b_just

textfield :: String -> HtmlM (Behaviour String)
textfield n = input ! type_ "text" ! name n

submit :: HtmlM (Behaviour String)
submit = input ! type_ "submit"

(~<) :: BehaviourFun a Int -> BehaviourFun a Int -> BehaviourFun a Bool
(~<) = binOp "lt_int"

x ~>= y = b_not (x ~< y)

b_length :: BehaviourFun a [b] -> BehaviourFun a Int
b_length = unOp "length"

b_fromJust :: Behaviour (Maybe b) -> Behaviour b
b_fromJust = b_maybe (b_error "fromJust: Nothing") id

b_lookup' :: (JSValue a, JSValue b) => BehaviourFun (a, [(a, b)]) (Maybe b)
b_lookup' = primFunc "lookup"
b_lookup = bcurry b_lookup'

b_not :: Behaviour Bool -> Behaviour Bool
b_not = b_bool b_false b_true

(~&&) :: Behaviour Bool -> Behaviour Bool -> Behaviour Bool
x ~&& y = b_bool y x x

(~==) :: Behaviour String -> Behaviour String -> Behaviour Bool
(~==) = binOp "eq_string"

b_fmap :: (Behaviour a -> Behaviour b) -> (Behaviour (Maybe a)) -> (Behaviour (Maybe b))
b_fmap f = b_maybe b_nothing (b_just . f)

b_join :: Behaviour (Maybe (Maybe a)) -> Behaviour (Maybe a)
b_join = b_maybe b_nothing id

b_return :: Behaviour a -> Behaviour (Maybe a)
b_return = b_just

(~>>=) :: Behaviour (Maybe a) -> (Behaviour a -> Behaviour (Maybe b)) -> Behaviour (Maybe b)
x ~>>= f = b_maybe b_nothing f x

b_liftM2 f mx my = mx ~>>= \x -> my ~>>= \y -> b_return (f x y)

b_uncurry :: (Behaviour a -> Behaviour b -> Behaviour c) -> Behaviour (a, b) -> Behaviour c
b_uncurry f = binOp "uncurry" (cb $ haskToBhv $ cb . haskToBhv . f)

--b_pair :: Behaviour a -> Behaviour b -> Behaviour (a, b)

--b_maybePair :: Behaviour (Maybe a, Maybe b) -> Behaviour (Maybe (a,b))
--b_maybePair = b_uncurry $ \mx my -> mx ~>>= \x -> my ~>>= \y -> b_return (x, y)



data RenderState = RenderState { rsUniq :: Int, rsJavascript :: String }
type RenderMonad a = Writer String a


render :: Html -> String
render html = let (HtmlM f) = renderH html
                  ((result, ()), _) = f (HtmlState 1 [] [])
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
