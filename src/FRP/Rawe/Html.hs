{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module FRP.Rawe.Html (
    -- * Data types
    --
    -- | The ones for representing JavaScript values are taken from the package
    -- json; those for HTML values and attribuets are ours.

    JSString, JSObject, JSValue, Result(..),
    HtmlM, Html, Attribute(..), (!),

    -- * HTML manipulation

    -- ** Converting to HTML

    ToHtmlBhv(..), BJSON(..),

    -- ** Embedding behaviours

    bhv,

    -- * Functions for HTML and JavaScript

    -- ** Result constructors and destructor

    result_ok, result_error, result,

    -- ** Manipulating JSON

    toJSObject', toJSObject, fromJSObject', fromJSObject,

    -- ** Other functions

    typeof', typeof,


    -- * Page construction

    -- ** Talking to a server

    sget', sget,
    post', post,

    -- ** Special values

    jquery, reactive, reactivePrim,
    initReactive,

    -- ** HTML elements

    a, ae,
    body, br, button,
    div_,
    form, form',
    head_, html,
    li,
    textfield', textfield, title,
    script, span_, submit, submit',
    ul,

    -- ** HTML attribuets

    name,
    src, style,
    type_,
    value,

    -- ** Other functions

    str,
    appendHtml,
    until,
) where


import Prelude hiding (div, (.), fst, snd, id, span, until)

import Control.Category
import Control.Category.Cartesian

import Control.Monad.State

import Data.String

import Text.JSON (JSString, JSObject, JSValue, Result(..))
import qualified Text.JSON as J


import FRP.Rawe
import FRP.Rawe.Internal
import qualified FRP.Rawe.Prelude as R


--------------------------------------------------------------------------------
-- HTML manipulation
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--   Converting to HTML

class ToHtmlBhv a where
    toHtml :: Bhv a -> Bhv Html

instance ToHtmlBhv Html where
    toHtml = id

instance ToHtmlBhv Int where
    toHtml = primOp1 (span_.str.show) "to_html_int"

instance ToHtmlBhv String where
    toHtml = toHtml . toJSString

instance ToHtmlBhv JSString where
    toHtml = primOp1 (span_ . str . J.fromJSString) "to_html_jsstring"


instance ToHtmlBhv [Html] where
    toHtml = R.foldl appendHtml (cb $ div_ $ return ())

instance ToHtmlBhv a => ToHtmlBhv (Maybe a) where
    toHtml = R.maybe (cb $ div_ $ return ()) toHtml


--------------------------------------------------------------------------------
--  Embedding behaviours

class BehaviourToHtml a where
    bhv :: Bhv (HtmlM a) -> HtmlM a

instance BehaviourToHtml () where
    bhv x = do ~(_,id) <- addBehaviour x
               cur <- gets hsHtmlCurrent
               HtmlM $ \s -> ((), ([Behaviour id], s { hsHtmlBehaviours = (id, cur) : hsHtmlBehaviours s } ))

instance BehaviourToHtml (Bhv a) where
    bhv x = do ~(r,id) <- addBehaviour x
               cur <- gets hsHtmlCurrent
               jbhv <- bhvValue (Assigned (error "eval: bhv") (r,id) :: Bhv (HtmlM (Bhv a)))
               nid <- addBehaviourName "bhv_to_html_inner" [jbhv]
               HtmlM $ \s -> (Assigned (error "eval: bhv inner") nid, ([Behaviour id], s { hsHtmlBehaviours = (id, cur) : hsHtmlBehaviours s } ))


instance BhvValue Html where
    bhvValue html = do
        (i, (raw, ())) <- withHtmlCurrent $ renderH html
        modify $ \s -> s { hsHtmlValues = (i, raw, Nothing) : hsHtmlValues s }
        return.RawJS $ "rawe.cthunk(r_html_"++show i++")"

instance BhvValue (HtmlM (Bhv a)) where
    bhvValue html = do
        (i, (raw, b)) <- withHtmlCurrent $ renderH html
        (RawJS inner) <- bhvValue b
        modify $ \s -> s { hsHtmlValues = (i, raw, Just inner) : hsHtmlValues s }
        return.RawJS $ "rawe.cthunk(r_html_"++show i++")"


withHtmlCurrent :: HtmlM a -> HtmlM (Int, a)
withHtmlCurrent act = do
    i <- htmlUniq
    orig <- get
    put $ orig { hsHtmlCurrent = Just i }
    res <- act
    modify $ \s -> s { hsHtmlCurrent = hsHtmlCurrent orig }
    return (i, res)



instance IsString (Bhv Html) where
    fromString = cb . span_ . fromString


--------------------------------------------------------------------------------
-- Functions and data types for HTML and JavaScript
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--  JavaScript values

class BJSON a where
    readJSON :: Bhv JSValue -> Bhv (Result a)
    writeJSON :: Bhv a -> Bhv JSValue

instance BJSON Int where
    readJSON x = R.ite (typeof x R.== "number") (result_ok $ unsafeCoerce x) (result_error "readJSON: not a number")
    writeJSON = unsafeCoerce

instance R.BEq JSString where
    (==) = primOp2 (==) "eq"


--------------------------------------------------------------------------------
--  Result constructors and destructor

result_ok :: Bhv a -> Bhv (Result a)
result_ok = primOp1 Ok "result_ok"

result_error :: Bhv String -> Bhv (Result a)
result_error = primOp1 Error "result_error"

result :: (Bhv a -> Bhv b) -> (Bhv String -> Bhv b) -> Bhv (Result a) -> Bhv b
result = primOp3 (\o e r -> case r of Ok x -> o x; Error s -> e s) "result"


--------------------------------------------------------------------------------
--  JSON types, functions and instances

toJSObject' :: Bhv [(JSString, a)] -> Bhv (JSObject a)
toJSObject' = primOp1 (J.toJSObject . map (\(x,y) -> (J.fromJSString x, y))) "to_js_object"

toJSObject :: Bhv [(String, a)] -> Bhv (JSObject a)
toJSObject = toJSObject' . R.map (\x -> toJSString (fst . x) &&& (snd . x))

fromJSObject' :: Bhv (JSObject a) -> Bhv [(JSString, a)]
fromJSObject' = primOp1 (map (\(x,y) -> (J.toJSString x, y)) . J.fromJSObject) "from_js_object"

fromJSObject :: Bhv (JSObject a) -> Bhv [(String, a)]
fromJSObject = R.map (\x -> fromJSString (fst . x) &&& (snd . x)) . fromJSObject'

instance R.BFunctor JSObject where
    fmap = primOp2 (\f -> J.toJSObject . map (fmap f) . J.fromJSObject) "js_object_fmap"


--------------------------------------------------------------------------------
--  Other functions

unsafeCoerce :: Bhv a -> Bhv b
unsafeCoerce = (.) (Assigned (error "eval: unsafeCoerce") (0,0))

typeof' :: Bhv JSValue -> Bhv JSString
typeof' = primOp1 (error "eval: typeof") "js_typeof"

typeof :: Bhv JSValue -> Bhv String
typeof = fromJSString . typeof'



--------------------------------------------------------------------------------
-- Page construction
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--  Talking to server

sget' :: String -> Bhv (Maybe JSValue)
sget' name = Prim (const Nothing) $ do
    jname <- bhvValue $ J.toJSString name
    return ("sget", [jname])

sget :: BJSON a => String -> Bhv (Maybe a)
sget = R.join . R.fmap (result R.just (const R.nothing) . readJSON) . sget'

post' :: String -> Event (JSObject JSString) -> HtmlM (Bhv (Maybe JSValue))
post' name signal = do jname <- bhvValue $ J.toJSString name
                       jsignal <- bhvValue signal
                       fmap (Assigned (const Nothing)) $ addBehaviourName "post" [jname, jsignal]


post :: (BJSON a) => String -> Event [(String,String)] -> HtmlM (Bhv (Maybe a))
post name = fmap (R.join . R.fmap (result R.just (const R.nothing) . readJSON)) .
    post' name . R.fmap (toJSObject . R.fmap (R.fmap toJSString))



--------------------------------------------------------------------------------
--  Special values

jquery :: Html
jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""

reactive :: Html
reactive = script ! type_ "text/javascript" ! src "js/reactive.js" $ ""

reactivePrim :: Html
reactivePrim = script ! type_ "text/javascript" ! src "js/reactive-prim.js" $ ""

initReactive :: Html
initReactive = do
    bs <- gets $ reverse . hsBehaviours
    hs <- gets $ reverse . hsHtmlValues
    hbs <- gets $ reverse . hsHtmlBehaviours
    hgs <- gets $ reverse . hsHtmlGens
    script ! type_ "text/javascript" $ do
        str $ "$(document).ready(function() {\n"
        str $ "var r_bhv_fun_0 = {};\n";

        forM_ bs $ \(i, func, params) ->
            str $ "r_bhv_fun_0["++show i++"] = new rawe.BhvFun("++show i++");\n"

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
                jfunc = "rawe.prim."++func
                jparams = concatMap ((',':).unRawJS) params
            str $ jfunc++".call(r_bhv_fun_0["++jid++"]"++jparams++");\n"

        str $ "rawe.init(r_bhv_fun_0);\n"
        str $ "});\n";



--------------------------------------------------------------------------------
--  Html elements


container :: String -> Html -> Html
container tag (HtmlM f) = HtmlM $ \s -> let ((), (content, s')) = f s
                                         in ((), ([Tag tag [] content], s'))

tag :: String -> Html
tag name = HtmlM $ \s -> ((), ([TagVoid name []], s))


htmlGen :: String -> ([HtmlStructure] -> HtmlStructure) -> HtmlM b -> HtmlM (Bhv a)
htmlGen name tag (HtmlM f) = do
    bid@(~(_,i)) <- addBehaviourName ("gen_"++name) []
    cur <- gets hsHtmlCurrent
    HtmlM $ \s -> let (_, (content, s')) = f s
                   in (Assigned (error "eval: htmlGen") bid, ([addAttr (AttrVal "bhv-gen" (show i)) (tag content)],
                       s' { hsHtmlGens = (i, cur) : hsHtmlGens s' } ))

input :: String -> HtmlM (Bhv a)
input t = htmlGen ("input_"++t) (\_ -> TagVoid "input" [AttrVal "type" t]) (return ())


doctype :: Html
doctype = HtmlM $ \s -> ((), ([Doctype], s))


a :: Html -> Html
a = container "a"

-- | Inserts an <a> element with href set to "#" and generates an event
-- triggering whenever is this element clicked on, sampling the value of the
-- behaviour from the first parameter.

ae :: Bhv String -> Html -> HtmlM (Event String)
ae s = fmap (R.fmap $ const s) . htmlGen "ae" (Tag "a" [AttrVal "href" "#"])

body :: Html -> Html
body content = container "body" $ do
    content
    initReactive

br :: Html
br = tag "br"

button :: HtmlM (Event ())
button = input "button"

div_ :: Html -> Html
div_ = container "div"

form :: HtmlM a -> HtmlM (Event [(String, String)])
form = fmap (R.fmap (fromJSObject . R.fmap fromJSString)) . form'

form' :: HtmlM a -> HtmlM (Event (JSObject JSString))
form' = htmlGen "form" (Tag "form" [])

head_ :: Html -> Html
head_ content = container "head" $ do
    jquery
    reactive
    reactivePrim
    content

html :: Html -> Html
html content = doctype >> container "html" content

li :: Html -> Html
li = container "li"

textfield' :: String -> HtmlM (Bhv JSString)
textfield' n = input "text" ! name n

textfield :: String -> HtmlM (Bhv String)
textfield = fmap fromJSString . textfield'

title :: Html -> Html
title = container "title"

script :: Html -> Html
script = container "script"

span_ :: Html -> Html
span_ = container "span"

submit :: HtmlM (Event String)
submit = fmap (R.fmap fromJSString) $ submit'

submit' :: HtmlM (Event JSString)
submit' = input "submit"

ul :: Html -> Html
ul = container "ul"


--------------------------------------------------------------------------------
--  Html attributes


name = AttrVal "name"
src = AttrVal "src"
style = AttrVal "style"
type_ = AttrVal "type"
value = AttrVal "value"



appendHtml :: Bhv Html -> Bhv Html -> Bhv Html
appendHtml = primOp2 (>>) "append_html"

until :: Bhv (HtmlM a) -> Bhv (Maybe Html) -> Bhv (HtmlM a)
until x m = prim $ BhvModifier2 (error "eval: until") "html_until" x m
