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

    -- * Converting to HTML

    ToHtmlBhv(..), BJSON(..),


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

    -- ** HTML elements

    head_, body,
    a, br, div, html, li, title, ul,

    form', form,
    textfield', textfield,
    submit', submit,
    button,

    -- ** HTML attribuets

    name, style, value,

    -- ** Other functions

    str,
    guardTimed, t2m,
    appendHtml,
    until,
) where


import Prelude hiding (div, (.), fst, snd, id, until)

import Control.Category
import Control.Category.Cartesian

import Control.Monad.State

import Text.JSON (JSString, JSObject, JSValue, Result(..))
import qualified Text.JSON as J


import FRP.Rawe
import FRP.Rawe.Internal
import qualified FRP.Rawe.Prelude as R



class ToHtmlBhv a where
    toHtml :: Bhv a -> Bhv Html

instance ToHtmlBhv Html where
    toHtml = id

instance ToHtmlBhv Int where
    toHtml = unOp "to_html_int"

instance ToHtmlBhv String where
    toHtml = toHtml . toJSString

instance ToHtmlBhv JSString where
    toHtml = unOp "to_html_jsstring"


instance ToHtmlBhv [Html] where
    toHtml = R.foldl appendHtml (cb $ div $ return ())

instance ToHtmlBhv a => ToHtmlBhv (Maybe a) where
    toHtml = R.maybe (cb $ div $ return ()) toHtml


instance R.BFunctor Timed where
    fmap f = timed notYet (\t x -> onTime t (f x))


class BJSON a where
    readJSON :: Bhv JSValue -> Bhv (Result a)
    writeJSON :: Bhv a -> Bhv JSValue

instance BJSON Int where
    readJSON x = R.ite (typeof x R.== "number") (result_ok $ unsafeCoerce x) (result_error "readJSON: not a number")
    writeJSON = unsafeCoerce


toJSString :: Bhv String -> Bhv JSString
toJSString = b_toJSString

fromJSString :: Bhv JSString -> Bhv String
fromJSString = b_fromJSString

instance R.BEq JSString where
        (==) = binOp "eq"


{- Result constructors and destructor -}

result_ok :: Bhv a -> Bhv (Result a)
result_ok = unOp "result_ok"

result_error :: Bhv String -> Bhv (Result a)
result_error = unOp "result_error"

result :: (Bhv String -> Bhv b) -> (Bhv a -> Bhv b) -> Bhv (Result a) -> Bhv b
result a b = terOp "result" (cb $ haskToBhv a) (cb $ haskToBhv b)


{- JSON types, functions and instances -}

toJSObject' :: Bhv [(JSString, a)] -> Bhv (JSObject a)
toJSObject' = unOp "to_js_object"

toJSObject :: Bhv [(String, a)] -> Bhv (JSObject a)
toJSObject = toJSObject' . R.map (\x -> toJSString (fst . x) &&& (snd . x))

fromJSObject' :: Bhv (JSObject a) -> Bhv [(JSString, a)]
fromJSObject' = unOp "from_js_object"

fromJSObject :: Bhv (JSObject a) -> Bhv [(String, a)]
fromJSObject = R.map (\x -> fromJSString (fst . x) &&& (snd . x)) . fromJSObject'

instance R.BFunctor JSObject where
        fmap f = binOp "js_object_fmap" (cb $ haskToBhv f)


{- Other functions -}

unsafeCoerce :: Bhv a -> Bhv b
unsafeCoerce = (.) (Assigned (0,0))

typeof' :: Bhv JSValue -> Bhv JSString
typeof' = unOp "typeof"

typeof :: Bhv JSValue -> Bhv String
typeof = fromJSString . typeof'




sget' :: String -> Bhv (Maybe JSValue)
sget' = Prim . BhvServer "sget" . J.toJSString

sget :: BJSON a => String -> Bhv (Maybe a)
sget = R.join . R.fmap (result (const R.nothing) R.just . readJSON) . sget'

post' :: String -> Bhv (Timed (JSObject JSString)) -> HtmlM (Bhv (Maybe JSValue))
post' name signal = do jname <- bhvValue $ J.toJSString name
                       jsignal <- bhvValue signal
                       fmap Assigned $ addBehaviourName "post" [jname, jsignal]


post :: (BJSON a) => String -> Bhv (Timed [(String,String)]) -> HtmlM (Bhv (Maybe a))
post name = fmap (R.join . R.fmap (result (const R.nothing) R.just . readJSON)) .
    post' name . R.fmap (toJSObject . R.fmap (R.fmap toJSString))






head_ content = container "head" $ do
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
title = container "title"
ul = container "ul"


htmlGen :: String -> ([HtmlStructure] -> HtmlStructure) -> HtmlM b -> HtmlM (Bhv a)
htmlGen name tag (HtmlM f) = do
    bid@(~(_,i)) <- addBehaviourName ("gen_"++name) []
    cur <- gets hsHtmlCurrent
    HtmlM $ \s -> let (_, (content, s')) = f s
                   in (Assigned bid, ([addAttr (AttrVal "bhv-gen" (show i)) (tag content)],
                       s' { hsHtmlGens = (i, cur) : hsHtmlGens s' } ))

input :: String -> HtmlM (Bhv a)
input t = htmlGen ("input_"++t) (Tag "input" [AttrVal "type" t]) (return ())

form' :: HtmlM a -> HtmlM (Bhv (Timed (JSObject JSString)))
form' = htmlGen "form" (Tag "form" [])

form :: HtmlM a -> HtmlM (Bhv (Timed [(String, String)]))
form = fmap (R.fmap (fromJSObject . R.fmap fromJSString)) . form'

textfield' :: String -> HtmlM (Bhv JSString)
textfield' n = input "text" ! name n

textfield :: String -> HtmlM (Bhv String)
textfield = fmap fromJSString . textfield'

submit' :: HtmlM (Bhv (Timed JSString))
submit' = input "submit"

submit :: HtmlM (Bhv (Timed String))
submit = fmap (R.fmap fromJSString) $ submit'

button :: HtmlM (Bhv (Timed ()))
button = input "button"



name = AttrVal "name"
style = AttrVal "style"
value = AttrVal "value"



guardTimed :: (Bhv a -> Bhv Bool) -> Bhv (Timed a) -> Bhv (Timed a)
guardTimed f tx = R.ite (timed R.false (const f) tx) tx notYet

t2m :: Bhv (Timed a) -> Bhv (Maybe a)
t2m = timed R.nothing (const R.just)

appendHtml :: Bhv Html -> Bhv Html -> Bhv Html
appendHtml = binOp "append_html"

until :: Bhv (HtmlM a) -> Bhv (Maybe Html) -> Bhv (HtmlM a)
until x m = Prim $ BhvModifier2 "html_until" x m
