{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


-- Html.hs: combinators for page construction and other HTML-related functions
-- as part of rawe - ReActive Web Framework
--
-- Copyright 2011 Roman Smrž <roman.smrz@seznam.cz>, see file LICENSE

-----------------------------------------------------------------------------
-- |
-- Maintainer  : Roman Smrž <roman.smrz@seznam.cz>
-- Stability   : experimental
--
-- This module provides all the available combinators for constructing web
-- pages. The list of provided elements is rather comprehensive, but we define
-- only a few attributes, although new ones may be define by the function
-- 'attr'.
--
-- Apart from that, here are to be found also various other HTML-related
-- function, notably those for converting other behaviour into HTML and
-- embedding them into a page. Also the functions for communicating with server
-- are to be found here.



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

    -- | Many HTML elements are provided here, but generation of behaviours or
    -- events is implemented only for some of them and not in every case where
    -- that would make sense

    a, ae, abbr, acronym, address, area,
    b, base, bdo, big, blockquote, body, br, button,
    caption, center, cite, code, col, colgroup,
    dd, del, dfn, dir_, div_, dl, dt,
    em,
    fieldset, font, form, form', frame, frameset,
    h1, head_, hr, html,
    i, iframe, img, input, ins, isindex,
    kbd,
    label, legend, li, link,
    map_, menu, meta, 
    noframes, noscript,
    object, ol, optgroup, option,
    p, param, pre, 
    q,
    s, samp, script, select, small, span_, strike, strong, sub, submit, submit', sup,
    table, tbody, td, textarea, textfield, textfield', tfoot, th, thead, title,
    tr, tt,
    u, ul,
    var,
    xmp,

    -- ** HTML attribuets

    -- | Functions for only a few attributes are predefined here currently;
    -- other may be generated using the attr function.

    attr,
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

-- | 'ToHtmlBhv' class defines those types which can be (in the form of
-- behaviours) converted to an HTML code. Instances are provided for some
-- types.

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

    -- ^ Embeds a behaviour into the page. It is part of a type class, because
    -- it is meat to work for two types: plain Html and HtmlM carrying some
    -- behaviour. For the latter, some mechanism for working with ''inner''
    -- behaviours is needed, provided by the bhv_to_html_inner prim.

instance BehaviourToHtml () where
    bhv x = do ~(_,id) <- assignBehaviour x
               cur <- gets hsHtmlCurrent
               HtmlM $ \s -> ((), ([Behaviour id], s { hsHtmlBehaviours = (id, cur) : hsHtmlBehaviours s } ))

instance BehaviourToHtml (Bhv a) where
    bhv x = do ~(r,id) <- assignBehaviour x
               cur <- gets hsHtmlCurrent
               jbhv <- bhvValue (Assigned (error "eval: bhv") (r,id) :: Bhv (HtmlM (Bhv a)))
               nid <- addBehaviourName "bhv_to_html_inner" [jbhv]
               HtmlM $ \s -> (Assigned (error "eval: bhv inner") nid, ([Behaviour id], s { hsHtmlBehaviours = (id, cur) : hsHtmlBehaviours s } ))


-- To make behaviours from values of types Html, we render in using the
-- 'renderH' function (for which we set the ''current'' HTML snippet to be the
-- one we are creating, using the helper function 'withHtmlCurrent'). We then
-- add it to the list of snippets it the inner state and return a code for
-- resulting thunk.

instance BhvValue Html where
    bhvValue html = do
        (i, (raw, ())) <- withHtmlCurrent $ renderH html
        modify $ \s -> s { hsHtmlValues = (i, raw, Nothing) : hsHtmlValues s }
        return.RawJS $ "rawe.cthunk(r_html_"++show i++")"

-- In the case of HtmlM carrying some inner behaviour, we also get the code for
-- that one and mark it to be set as a property for the created HTML snippet.

instance BhvValue (HtmlM (Bhv a)) where
    bhvValue html = do
        (i, (raw, b)) <- withHtmlCurrent $ renderH html
        (RawJS inner) <- bhvValue b
        modify $ \s -> s { hsHtmlValues = (i, raw, Just inner) : hsHtmlValues s }
        return.RawJS $ "rawe.cthunk(r_html_"++show i++")"


-- Helper function creating  ''local'' state for generating HTML snippet. It
-- generates a unique ID for the new HTML snippet and then set it to be the
-- ''current'' one - this is needed for 'bhv' to work and also for
-- ''generating'' elements. After performing action, resets the value to the
-- original.

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

-- | 'BJSON' class is similar to 'JSON' from the package json, only difference
-- being that it works with behaviours. It is used for parsing values received
-- from server in the case of GET and POST requests for data.

class BJSON a where
    readJSON :: Bhv JSValue -> Bhv (Result a)
    writeJSON :: Bhv a -> Bhv JSValue

instance BJSON Int where
    readJSON x = R.ite (typeof x R.== "number") (result_ok $ unsafeCoerce x) (result_error "readJSON: not a number")
    writeJSON = unsafeCoerce

instance R.BEq JSString where
    (==) = primOp2 (==) "cmp_eq"


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

-- | Behaviour-enabled variant of unsafeCoerce

unsafeCoerce :: Bhv a -> Bhv b
unsafeCoerce = (.) (Assigned (error "eval: unsafeCoerce") (0,0))

-- | Typeof calls the JavaScript typeof keyword and returns the result

typeof :: Bhv JSValue -> Bhv String
typeof = fromJSString . typeof'

-- | Raw 'JSString' variant of 'typeof'.

typeof' :: Bhv JSValue -> Bhv JSString
typeof' = primOp1 (error "eval: typeof") "js_typeof"



--------------------------------------------------------------------------------
-- Page construction
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--  Talking to server

-- | Behaviour 'get ''name''' gets a value from the server for a GET request ?q=name.

sget :: BJSON a => String -> Bhv (Maybe a)
sget = R.join . R.fmap (result R.just (const R.nothing) . readJSON) . sget'

-- | Raw 'JSValue' version of 'sget'.

sget' :: String -> Bhv (Maybe JSValue)
sget' name = Prim (const Nothing) $ do
    jname <- bhvValue $ J.toJSString name
    return ("sget", [jname])

-- | This function is registered somewhere to a HtmlM monad and then for each
-- occurrence of the event sends one POST request to the server. The resulting
-- behaviour is 'Just' with the value of last received response (or Nothing, if
-- none arrived yet). The embedding into HtmlM is used in order to guarantee
-- sending the correct number of requests regardless of evaluating of the
-- results.

post :: (BJSON a) => String -> Event [(String,String)] -> HtmlM (Bhv (Maybe a))
post name = fmap (R.join . R.fmap (result R.just (const R.nothing) . readJSON)) .
    post' name . R.fmap (toJSObject . R.fmap (R.fmap toJSString))

-- | Raw 'JSValue' variant of 'post'

post' :: String -> Event (JSObject JSString) -> HtmlM (Bhv (Maybe JSValue))
post' name signal = do jname <- bhvValue $ J.toJSString name
                       jsignal <- bhvValue signal
                       fmap (Assigned (const Nothing)) $ addBehaviourName "post" [jname, jsignal]



--------------------------------------------------------------------------------
--  Special values

-- | The script with jQuery source.

jquery :: Html
jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""

-- | The script with main part of Rawe JavaScript code.

reactive :: Html
reactive = script ! type_ "text/javascript" ! src "js/rawe.js" $ ""

-- | The script with the JavaScript primitives.

reactivePrim :: Html
reactivePrim = script ! type_ "text/javascript" ! src "js/rawe-prim.js" $ ""

-- | Code initializing the whole dynamic system

initReactive :: Html
initReactive = do

    -- Before generating code, we get all the relevart info from the state
    bs <- gets $ reverse . hsBehaviours
    hs <- gets $ reverse . hsHtmlValues
    hbs <- gets $ reverse . hsHtmlBehaviours
    hgs <- gets $ reverse . hsHtmlGens

    script ! type_ "text/javascript" $ do
        str $ "$(document).ready(function() {\n"
        str $ "var r_bhv_fun_0 = {};\n";

        -- First we create all the objects
        forM_ bs $ \(i, _, _) ->
            str $ "r_bhv_fun_0["++show i++"] = new rawe.BhvFun("++show i++");\n"

        -- Define HTML snippets, which are dynamically created
        forM_ hs $ \(i, h, mi) ->
            str $ "var r_html_"++show i++" = $('"++escapeStringJS h++"')" ++
            case mi of { Nothing -> ""; Just inner -> ".prop('rawe_html_inner', "++inner++")" }
            ++ ";\n"

        -- Assign to behaviour those HTML snippets for which they are responsible
        forM_ hbs $ \(i, mv) ->
            let var = case mv of Nothing -> "$('body')"
                                 Just v  -> "r_html_"++show v
             in str $ "r_bhv_fun_0["++show i++"].html = "++var++".find('*[bhv-id="++show i++"]');\n"

        -- Assign to behaviurs HTML elements, which they use to generate values / events.
        forM_ hgs $ \(i, mv) ->
            let var = case mv of Nothing -> "$('body')"
                                 Just v  -> "r_html_"++show v
             in str $ "r_bhv_fun_0["++show i++"].gen = "++var++".find2('*[bhv-gen="++show i++"]');\n"

        -- And finally call all the initialization functions.
        forM_ bs $ \(i, func, params) -> do
            let jid = show i
                jfunc = "rawe.prim."++func
                jparams = concatMap ((',':).unRawJS) params
            str $ jfunc++".call(r_bhv_fun_0["++jid++"]"++jparams++");\n"

        str $ "rawe.init(r_bhv_fun_0);\n"
        str $ "});\n";



--------------------------------------------------------------------------------
--  Html elements

-- | Functin creating non-void elements.

container :: String -> Html -> Html
container tag (HtmlM f) = HtmlM $ \s -> let ((), (content, s')) = f s
                                         in ((), ([Tag tag [] content], s'))


-- | Function creating void elements.

tag :: String -> Html
tag name = HtmlM $ \s -> ((), ([TagVoid name []], s))


-- | General initializer of behaviour-generating elements.

htmlGen :: String -> ([HtmlStructure] -> HtmlStructure) -> HtmlM b -> HtmlM (Bhv a)
htmlGen name tag (HtmlM f) = do
    bid@(~(_,i)) <- addBehaviourName ("gen_"++name) []
    cur <- gets hsHtmlCurrent
    HtmlM $ \s -> let (_, (content, s')) = f s
                   in (Assigned (error "eval: htmlGen") bid, ([addAttr (AttrVal "bhv-gen" (show i)) (tag content)],
                       s' { hsHtmlGens = (i, cur) : hsHtmlGens s' } ))

-- | Specialization of 'htmlGen' to input elements.

inputGen :: String -> HtmlM (Bhv a)
inputGen t = htmlGen ("input_"++t) (\_ -> TagVoid "input" [AttrVal "type" t]) (return ())


-- | The doctype.

doctype :: Html
doctype = HtmlM $ \s -> ((), ([Doctype], s))


-- Following are all the provided functions for HTML elements:


a :: Html -> Html
a = container "a"

-- | Inserts an <a> element with href set to "#" and generates an event
-- triggering whenever is this element clicked on, sampling the value of the
-- behaviour from the first parameter.

ae :: Bhv String -> Html -> HtmlM (Event String)
ae s = fmap (R.fmap $ const s) . htmlGen "ae" (Tag "a" [AttrVal "href" "#"])

abbr :: Html -> Html
abbr = container "abbr"

acronym :: Html -> Html
acronym = container "acronym"

address :: Html -> Html
address = container "address"

area :: Html
area = tag "area"

b :: Html -> Html
b = container "b"

base :: Html
base = tag "base"

bdo :: Html -> Html
bdo = container "bdo"

big :: Html -> Html
big = container "big"

blockquote :: Html -> Html
blockquote = container "blockquote"

body :: Html -> Html
body content = container "body" $ do
    content
    initReactive

br :: Html
br = tag "br"

button :: HtmlM (Event ())
button = inputGen "button"

caption :: Html -> Html
caption = container "caption"

center :: Html -> Html
center = container "center"

cite :: Html -> Html
cite = container "cite"

code :: Html -> Html
code = container "code"

col :: Html
col = tag "col"

colgroup :: Html -> Html
colgroup = container "colgroup"

dd :: Html -> Html
dd = container "dd"

del :: Html -> Html
del = container "del"

dfn :: Html -> Html
dfn = container "dfn"

dir_ :: Html -> Html
dir_ = container "dir"

div_ :: Html -> Html
div_ = container "div"

dl :: Html -> Html
dl = container "dl"

dt :: Html -> Html
dt = container "dt"

em :: Html -> Html
em = container "em"

fieldset :: Html -> Html
fieldset = container "fieldset"

font :: Html -> Html
font = container "font"

form :: HtmlM a -> HtmlM (Event [(String, String)])
form = fmap (R.fmap (fromJSObject . R.fmap fromJSString)) . form'

form' :: HtmlM a -> HtmlM (Event (JSObject JSString))
form' = htmlGen "form" (Tag "form" [])

frame :: Html
frame = tag "frame"

frameset :: Html -> Html
frameset = container "frameset"

h1 :: Html -> Html
h1 = container "h1"

head_ :: Html -> Html
head_ content = container "head" $ do
    jquery
    reactive
    reactivePrim
    content

hr :: Html
hr = tag "hr"

html :: Html -> Html
html content = doctype >> container "html" content

i :: Html -> Html
i = container "i"

iframe :: Html -> Html
iframe = container "iframe"

img :: Html
img = tag "img"

input :: Html
input = tag "input"

ins :: Html -> Html
ins = container "ins"

isindex :: Html -> Html
isindex = container "isindex"

kbd :: Html -> Html
kbd = container "kbd"

label :: Html -> Html
label = container "label"

legend :: Html -> Html
legend = container "legend"

li :: Html -> Html
li = container "li"

link :: Html
link = tag "link"

map_ :: Html -> Html
map_ = container "map"

menu :: Html -> Html
menu = container "menu"

meta :: Html
meta = tag "meta"

noframes :: Html -> Html
noframes = container "noframes"

noscript :: Html -> Html
noscript = container "noscript"

object :: Html -> Html
object = container "object"

ol :: Html -> Html
ol = container "ol"

optgroup :: Html -> Html
optgroup = container "optgroup"

option :: Html -> Html
option = container "option"

p :: Html -> Html
p = container "p"

param :: Html
param = tag "param"

pre :: Html -> Html
pre = container "pre"

q :: Html -> Html
q = container "q"

s :: Html -> Html
s = container "s"

samp :: Html -> Html
samp = container "samp"

script :: Html -> Html
script = container "script"

select :: Html -> Html
select = container "select"

small :: Html -> Html
small = container "small"

span_ :: Html -> Html
span_ = container "span"

strike :: Html -> Html
strike = container "strike"

strong :: Html -> Html
strong = container "strong"

sub :: Html -> Html
sub = container "sub"

-- | Submin is an <input type="submit"> element; generating evens on triggering.

submit :: HtmlM (Event String)
submit = fmap (R.fmap fromJSString) $ submit'

submit' :: HtmlM (Event JSString)
submit' = inputGen "submit"

sup :: Html -> Html
sup = container "sup"

table :: Html -> Html
table = container "table"

tbody :: Html -> Html
tbody = container "tbody"

td :: Html -> Html
td = container "td"

textarea :: Html -> Html
textarea = container "textarea"

-- | Textfild is an <input type="text"> element; generating behaviour of its value

textfield :: HtmlM (Bhv String)
textfield = fmap fromJSString $ textfield'

textfield' :: HtmlM (Bhv JSString)
textfield' = inputGen "text"

tfoot :: Html -> Html
tfoot = container "tfoot"

th :: Html -> Html
th = container "th"

thead :: Html -> Html
thead = container "thead"

title :: Html -> Html
title = container "title"

tr :: Html -> Html
tr = container "tr"

tt :: Html -> Html
tt = container "tt"

u :: Html -> Html
u = container "u"

ul :: Html -> Html
ul = container "ul"

var :: Html -> Html
var = container "var"

xmp :: Html -> Html
xmp = container "xmp"


--------------------------------------------------------------------------------
--  Html attributes

-- | 'attr ''name'' ''value''' creates an attribute ''name'' with value ''value''.

attr :: String -> String -> Attribute
attr = AttrVal


-- Some predefined attributes:

name :: String -> Attribute
name = AttrVal "name"

src :: String -> Attribute
src = AttrVal "src"

style :: String -> Attribute
style = AttrVal "style"

type_ :: String -> Attribute
type_ = AttrVal "type"

value :: String -> Attribute
value = AttrVal "value"


--------------------------------------------------------------------------------
--  Other functions


-- | Appends two HTML snippets.

appendHtml :: Bhv Html -> Bhv Html -> Bhv Html
appendHtml = primOp2 (>>) "append_html"


-- | The function 'until' works similarly to '\x -> maybe x id', but keeps the
-- inner value of the x :: (Bhv HtmlM a) parameter even when the second one
-- becomes 'Just'.

until :: Bhv (HtmlM a) -> Bhv (Maybe Html) -> Bhv (HtmlM a)
until x m = prim $ BhvModifier2 (error "eval: until") "html_until" x m
