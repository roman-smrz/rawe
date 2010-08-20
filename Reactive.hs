{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, OverloadedStrings, TypeSynonymInstances, NoMonomorphismRestriction #-}

module Reactive where

import Prelude hiding (head)

import Control.Monad.State
import Control.Monad.Writer

import Data.String


data Attribute = AttrVal String String
               | AttrBool String
               | forall a. JSValue a => EventCall String (Event a) (Behaviour a)

class Attributable b where
        (!) :: b -> Attribute -> b


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


data Event a = Event Int

data HtmlEvent = OnClick


data Push = forall a. (JSValue a) => Push String (Event a)


data HtmlState = HtmlState { hsUniq :: Int, hsPushes :: [Push] }

data HtmlStructure = Tag String [Attribute] [HtmlStructure]
                   | Text String
                   | Behaviour (Behaviour Html)
                   | Placeholder Int [Attribute]

data HtmlM a = HtmlM (HtmlState -> (a, ([HtmlStructure], HtmlState)))

type Html = HtmlM ()



instance Monad HtmlM where
        return x = HtmlM $ \s -> (x, ([], s))
        (HtmlM f) >>= g = HtmlM $ \s -> let (x, (cs, s')) = f s
                                            (HtmlM g') = g x
                                            (y, (ds, s'')) = g' s'
                                         in (y, (cs++ds, s''))

instance MonadState HtmlState HtmlM where
        get = HtmlM $ \s -> (s, ([], s))
        put s = HtmlM $ \_ -> ((), ([], s))


instance IsString Html where
        fromString text = HtmlM $ \s -> ((), ([Text text], s))


instance Attributable (HtmlM a) where
        (HtmlM f) ! a = HtmlM $ \s -> let (x, (cs, s')) = f s
                                       in (x, (map (addAttr a) cs, s'))

instance Attributable (HtmlM a -> HtmlM a) where
        f ! a = \html -> HtmlM $ \s -> let (HtmlM g) = f html
                                           (x, (t, s')) = g s
                                        in (x, (map (addAttr a) t, s'))


addAttr :: Attribute -> HtmlStructure -> HtmlStructure
addAttr a (Tag name as content) = Tag name (a:as) content
addAttr _ t@(Text _) = t
addAttr a (Behaviour b) = Behaviour $ Func (AddAttr a) b
addAttr a (Placeholder p as) = Placeholder p (a:as)

data AddAttr = AddAttr Attribute
instance JSFunc AddAttr Html Html where
        jsFunc (AddAttr (AttrVal name val)) = renderPutJS $ "return param.attr("++show name++", "++show val++")"
        jsFunc (AddAttr (AttrBool name)) = renderPutJS $ "return param.attr("++show name++", true)"
        jsFunc (AddAttr (EventCall ('o':'n':name) (Event e) val)) = do
                jsval <- jsValue val
                renderPutJS $ "return param."++name++"(function() { call_event("++show e++", "++jsval++"); });"



mkEvent :: (JSValue a) => HtmlM (Event a)
mkEvent = return.Event =<< htmlUniq




--mapb :: (a -> b) -> Behaviour [a] -> Behaviour [b]


container :: String -> Html -> Html
container tag (HtmlM f) = HtmlM $ \s -> let ((), (content, s')) = f s
                                         in ((), ([Tag tag [] content], s'))



html = container "html"
head = container "head"
title = container "title"
body = container "body"
ul = container "ul"
li = container "li"
script = container "script"

type_ = AttrVal "type"
src = AttrVal "src"

onclick = EventCall "onclick"


data StringToHtml = StringToHtml String
instance JSFunc StringToHtml String Html where
        jsFunc (StringToHtml text) = error "StringToHtml: not implemented"

instance JSFunc (Html -> Html) Html Html where
        jsFunc f = do
                pl <- renderUniq
                val <- jsValue . f $ HtmlM $ \s -> ((), ([Placeholder pl []], s))
                renderPutJS $ "var result = "++val++".clone(); result.find('div[placeholder-id="++show pl++"]').replaceWith(param); return result;"


data ToHtmlInt = ToHtmlInt
instance JSFunc ToHtmlInt Int Html where
        jsFunc f = renderPutJS $ "return $('<span>'+param+'</span>');"

msg2li :: String -> Html
msg2li = li . fromString



jquery :: Html
jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""

jsinit :: Html
jsinit = script ! type_ "text/javascript" $ "\
		\$(document).ready(function() {\
		\  $('div.behaviour-placeholder').each(function() {\
		\    var expr = $(this).attr('expr');\
		\    $(this).replaceWith(eval(expr));\
		\  });\
		\});\
                \ "


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
                        HtmlM $ \s -> ((), ([Behaviour x], s))
                        li $ "polozka"



data RenderState = RenderState { rsUniq :: Int, rsJavascript :: String }

type RenderMonad a = StateT RenderState (Writer String) a


render :: Html -> String
render (HtmlM xs) = snd $ runWriter $ evalStateT (mapM render' $ fst $ snd $ xs $ HtmlState 1 []) (RenderState 1 "")
        where render' :: HtmlStructure -> RenderMonad ()

              render' (Tag tag attrs xs) = do
                      tell $ "<" ++ tag 
                      renderAttrs attrs
                      tell ">\n"
                      mapM render' xs
                      tell $ "</" ++ tag ++ ">\n"

              render' (Text text) = tell text
              render' (Behaviour b) = do
                      val <- jsValue b
                      tell $ "<div class=\"behaviour-placeholder\" expr=\""++val++"\"></div>\n"
                      gets (not.null.rsJavascript) >>= flip when renderJavascript
              render' (Placeholder p attrs) = do
                      tell $ "<div placeholder-id=\""++show p++"\""
                      renderAttrs attrs
                      tell "></div>\n"

              renderAttrs [] = return ()
              renderAttrs ((AttrBool name) : rest) = tell (' ':name) >> renderAttrs rest
              renderAttrs ((AttrVal name val) : rest) = tell (" "++name++"=\""++val++"\"") >> renderAttrs rest
              renderAttrs ((EventCall name (Event id) value) : rest) = do
                      param <- jsValue value
                      tell $ " "++name++"=\"run_event("++show id++","++param++")\""
                      renderAttrs rest

{-
              renderEvents [] = return ()
              renderEvents ((OnClick, EventDesc name value) : rest) = do
                      param <- jsValue value
                      tell $ " onclick=\"run_event('"++name++"',"++param++")\""
                      renderEvents rest
                      -}

              renderJavascript = do
                      tell "<script type=\"text/javascript\">\n"
                      tell =<< gets rsJavascript
                      tell "</script>\n"



htmlUniq :: HtmlM Int
htmlUniq = do { x <- gets hsUniq; modify $ \s -> s { hsUniq = x+1 }; return x }

renderUniq :: RenderMonad Int
renderUniq = do { x <- gets rsUniq; modify $ \s -> s { rsUniq = x+1 }; return x }

renderPutJS :: String -> RenderMonad String
renderPutJS impl = do
        name <- return.("func_"++).show =<< renderUniq
        modify $ \s -> s { rsJavascript = rsJavascript s ++ "function "++name++"(param) {"++impl++"}\n" }
        return name
