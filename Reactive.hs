{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, OverloadedStrings, TypeSynonymInstances #-}

module Reactive where

import Prelude hiding (head)

import Control.Monad.State
import Control.Monad.Writer

import Data.String


data Attribute = AttrVal String String
               | AttrBool String

{-
data Func a b = Func (a -> b) String

data WebBehaviour a = WebBehaviour [WebBehaviour' a]

data WebBehaviour' a = Tag String [Attribute] (WebBehaviour a)
                     | Text String
                     | Value a
                     -- | SrvVal String
                     | forall b. Trans (Func b a) (WebBehaviour b)
                     | forall b. Bind (Func b (WebBehaviour a)) (WebBehaviour b)

data SrvVal a = SrvVal String

srvval name = WebBehaviour [Value $ SrvVal name]

instance Monad WebBehaviour where
        return x = WebBehaviour [Value x]
        (Tag tag attrs xs) >>= f  =  Tag tag attrs $ map (>>=f) xs
        (Text text) >>= _  =  Text text
        (Value x) >>= f  =  f x
        wb >>= f = Bind (Func f undefined) wb


container tag (WebBehaviour bs) = WebBehaviour [Tag tag [] bs]
-}

class JSFunc f a b | f -> a b where
        jsFunc :: f -> RenderMonad String

class JSValue a where
        jsValue :: a -> RenderMonad String

instance JSValue Int where jsValue = return.show
instance JSValue String where jsValue = return.show
instance JSValue Html where jsValue html = return $ "$('"++escapeStringJS(render html)++"')"

escapeStringJS = (>>=helper)
        where helper '\n' = "\\n"
              helper c | c `elem` "'\"\\" = '\\':c:[]
                       | otherwise        = [c]


data Behaviour a = Value a
                 | SrvVal String
                 | forall b f. (JSValue b, JSFunc f b a) => Func f (Behaviour b)

instance (JSValue a) => JSValue (Behaviour a) where
        jsValue (Value x) = jsValue x
        jsValue (SrvVal name) = return $ "srv_val('"++name++"')"
        jsValue (Func f b) = do
                func <- jsFunc f
                param <- jsValue b
                return $ func++"(" ++ param ++ ")"


data EventDesc = forall a. JSValue a => EventDesc String (Behaviour a)

data Event a = Event Int

data HtmlEvent = OnClick


data Push = forall a. (JSValue a) => Push String (Event a)


data HtmlState { hsUniq :: Int, hsPushes :: [Push] }

data HtmlM' a = Tag String [Attribute] [(HtmlEvent, EventDesc)] (HtmlM a)
              | Text String
              | Behaviour (Behaviour Html)
              | Placeholder Int
              | HtmlReturn a

data HtmlM a = HtmlM [HtmlM' a]

type Html = StateT Int HtmlM ()



instance Monad HtmlM where
        return = HtmlM . (:[]) . HtmlReturn
        (HtmlM [HtmlReturn a]) >>= f = f a
        (HtmlM xs) >>= f = let (HtmlM ys) = f undefined in HtmlM (convert xs ++ ys)
                where convert (Tag name attrs events (HtmlM content) : rest) = Tag name attrs events (HtmlM $ convert content) : convert rest
                      convert (Text text : rest) = Text text : convert rest
                      convert (Behaviour b : rest) = Behaviour b : convert rest
                      convert (Placeholder p : rest) = Placeholder p : convert rest
                      convert (HtmlReturn _ : rest) = convert rest
                      convert [] = []


instance IsString Html where
        fromString text = StateT $ \s -> (HtmlM [Text text], s)



--mapb :: (a -> b) -> Behaviour [a] -> Behaviour [b]


container :: String -> Html -> Html
container tag content = HtmlM [Tag tag [] [] content]



html = container "html"
head = container "head"
title = container "title"
body = container "body"
ul = container "ul"
li = container "li"


data StringToHtml = StringToHtml String
instance JSFunc StringToHtml String Html where
        jsFunc (StringToHtml text) = error "StringToHtml: not implemented"

instance JSFunc (Html -> Html) Html Html where
        jsFunc f = do
                pl <- renderGetID
                val <- jsValue . f $ HtmlM [Placeholder pl]
                renderPutJS $ "var result = "++val++"; result.find('div[placeholder-id="++show pl++"]').replaceWith(param); return result;"


data ToHtmlInt = ToHtmlInt
instance JSFunc ToHtmlInt Int Html where
        jsFunc f = renderPutJS $ "return $('<span>'+param+'</span>');"

msg2li :: String -> Html
msg2li = li . fromString



jquery :: Html
jquery = HtmlM [Tag "script" [AttrVal "type" "text/javascript", AttrVal "src" "jquery.js"] [] (HtmlM [Text ""])]

jsinit :: Html
jsinit = HtmlM [Tag "script" [AttrVal "type" "text/javascript"] [] $ HtmlM [Text "\
		\$(document).ready(function() {\
		\  $('div.behaviour-placeholder').each(function() {\
		\    var expr = $(this).attr('expr');\
		\    $(this).replaceWith(eval(expr));\
		\  });\
		\});\
                \ "
        ]]


page = html $ do
        head $ do
                title "Catopsis"
                jquery
        body $ do
                jsinit
                ul $ do
                        let msgs = SrvVal "msgs" :: Behaviour [String]
                        let count = Value 20 :: Behaviour Int
                        let x = Func li $ Func ToHtmlInt count :: Behaviour Html
                        HtmlM [Behaviour x]
                        li $ "polozka"



data RenderState = RenderState { rsUniq :: Int, rsJavascript :: String }

type RenderMonad a = StateT RenderState (Writer String) a


render :: Html -> String
render (HtmlM xs) = snd $ runWriter $ evalStateT (mapM render' xs) (RenderState 1 "")
        where render' :: HtmlM' () -> RenderMonad ()

              render' (Tag tag attrs events (HtmlM xs)) = do
                      tell $ "<" ++ tag 
                      renderAttrs attrs
                      renderEvents events
                      tell ">\n"
                      mapM render' xs
                      tell $ "</" ++ tag ++ ">\n"

              render' (Text text) = tell text
              render' (Behaviour b) = do
                      val <- jsValue b
                      tell $ "<div class=\"behaviour-placeholder\" expr=\""++val++"\"></div>\n"
                      gets (not.null.rsJavascript) >>= flip when renderJavascript
              render' (Placeholder p) = tell $ "<div placeholder-id=\""++show p++"\"></div>\n"

              renderAttrs [] = return ()
              renderAttrs ((AttrBool name) : rest) = tell (' ':name) >> renderAttrs rest
              renderAttrs ((AttrVal name val) : rest) = tell (" "++name++"=\""++val++"\"") >> renderAttrs rest

              renderEvents [] = return ()
              renderEvents ((OnClick, EventDesc name value) : rest) = do
                      param <- jsValue value
                      tell $ " onclick=\"run_event('"++name++"',"++param++")\""
                      renderEvents rest

              renderJavascript = do
                      tell "<script type=\"text/javascript\">\n"
                      tell =<< gets rsJavascript
                      tell "</script>\n"


renderGetID :: RenderMonad Int
renderGetID = do { x <- gets rsUniq; modify $ \s -> s { rsUniq = x+1 }; return x }

renderPutJS :: String -> RenderMonad String
renderPutJS impl = do
        name <- return.("func_"++).show =<< renderGetID
        modify $ \s -> s { rsJavascript = rsJavascript s ++ "function "++name++"(param) {"++impl++"}\n" }
        return name
