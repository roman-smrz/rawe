{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts,
  FunctionalDependencies, OverloadedStrings,
  TypeSynonymInstances, NoMonomorphismRestriction #-}

module Reactive where

import Prelude hiding (head,div)

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
        jsValueList :: [a] -> RenderMonad String
        jsValueList = return.("["++).(++"]").tail.concat <=< mapM (return.(',':) <=< jsValue)

instance (JSValue a) => JSValue [a] where
        jsValue = jsValueList

instance JSValue Int where jsValue = return.show
instance JSValue Html where jsValue html = return $ "$('"++escapeStringJS(render html)++"')"

instance JSValue Char where
        jsValue = return.show
        jsValueList = return.show

instance (JSValue a, JSValue b) => JSValue (a, b) where
        jsValue (x,y) = do
                x' <- jsValue x; y' <- jsValue y
                return $ "["++x'++", "++y'++"]"


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


data Behaviour a = Value a
                 | SrvVal String
                 | forall b f. (JSValue b, JSFunc f b a) => Func f (Behaviour b)
                 | forall b c f. (JSValue b, JSValue c, JSFunc f (b,c) a) => Func2 f (Behaviour b) (Behaviour c)

data BhvCacheKey = forall a.  BhvCacheKey (Behaviour a)
instance Eq BhvCacheKey where
        BhvCacheKey (SrvVal a) == BhvCacheKey (SrvVal b)  =  a == b
        _ == _ = False

instance (JSValue a) => JSValue (Behaviour a) where
        jsValue bhv = do
                mbid <- gets (lookup (BhvCacheKey bhv) . rsBhvCache)
                case mbid of
                     Just id  -> return $ show id
                     Nothing -> do
                             id <- renderUniq
                             modify $ \s -> s { rsBhvCache = (BhvCacheKey bhv, id) : rsBhvCache s }

                             case bhv of
                                  (Value x) -> do
                                          val <- jsValue x
                                          renderPutJS $ "r_bhv_val["++show id++"] = "++val++";"

                                  (SrvVal name) -> do
                                          renderPutJS $ "r_bhv_srv["++show id++"] = "++show name++";"

                                  (Func f b) -> do
                                          func <- jsFunc f
                                          id' <- jsValue b
                                          renderPutJS $ "r_bhv_deps["++show id++"] = ["++id'++"];"
                                          renderPutJS $ "r_bhv_func["++show id++"] = function() { return "++func++"(r_bhv_val["++id'++"]); };"

                                  (Func2 f b c) -> do
                                          func <- jsFunc f
                                          id1 <- jsValue b; id2 <- jsValue c
                                          renderPutJS $ "r_bhv_deps["++show id++"] = ["++id1++", "++id2++"];"
                                          renderPutJS $ "r_bhv_func["++show id++"] = function() { return "++func++"([r_bhv_val["++id1++"], r_bhv_val["++id2++"]]); };"

                             return $ show id


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
        jsFunc (AddAttr (AttrVal name val)) = renderPutJSFun $ "return param.attr("++show name++", "++show val++")"
        jsFunc (AddAttr (AttrBool name)) = renderPutJSFun $ "return param.attr("++show name++", true)"
        jsFunc (AddAttr (EventCall ('o':'n':name) (Event e) val)) = do
                jsval <- jsValue val
                renderPutJSFun $ "return param."++name++"(function() { call_event("++show e++", "++jsval++"); });"



mkEvent :: (JSValue a) => HtmlM (Event a)
mkEvent = return.Event =<< htmlUniq





container :: String -> Html -> Html
container tag (HtmlM f) = HtmlM $ \s -> let ((), (content, s')) = f s
                                         in ((), ([Tag tag [] content], s'))



a = container "a"
body = container "body"
div = container "div"
head = container "head"
html = container "html"
li = container "li"
script = container "script"
span = container "span"
title = container "title"
ul = container "ul"

type_ = AttrVal "type"
src = AttrVal "src"

onclick = EventCall "onclick"


instance JSFunc (Html -> Html) Html Html where
        jsFunc f = do
                pl <- renderUniq
                val <- jsValue . f $ HtmlM $ \s -> ((), ([Placeholder pl []], s))
                renderPutJSFun $ "var result = "++val++".clone(); result.find('div[placeholder-id="++show pl++"]').replaceWith(param); return result;"


data ToHtmlInt = ToHtmlInt
instance JSFunc ToHtmlInt Int Html where
        jsFunc f = renderPutJSFun $ "return $('<span>'+param+'</span>');"

data ToHtmlString = ToHtmlString
instance JSFunc ToHtmlString String Html where
        jsFunc f = renderPutJSFun $ "return $('<span>'+param+'</span>');"

data ToHtmlHtmlList = ToHtmlHtmlList
instance JSFunc ToHtmlHtmlList [Html] Html where
        jsFunc f = return "r_prim_toHtmlHtmlList"


data BhvMap a b = forall f. (JSValue a, JSValue b, JSFunc f a b) => BhvMap f
instance JSFunc (BhvMap a b) [a] [b] where
        jsFunc (BhvMap f) = do
                jsf <- jsFunc f
                renderPutJSFun $ "var result = []; for (var i in param) result[i] = "++jsf++"(param[i]); return result; "
bmap :: (JSValue a, JSValue b, JSFunc f a b) => f -> Behaviour [a] -> Behaviour [b]
bmap f = Func (BhvMap f)

data BhvLength a = (JSValue a) => BhvLength
instance JSFunc (BhvLength a) [a] Int where
        jsFunc BhvLength = renderPutJSFun $ "return param.length;"
blength :: (JSValue a) => Behaviour [a] -> Behaviour Int
blength = Func BhvLength

data BhvEnumFromTo = BhvEnumFromTo
instance JSFunc BhvEnumFromTo (Int,Int) [Int] where
        jsFunc BhvEnumFromTo = return "r_prim_enumFromTo"
benumFromTo :: Behaviour Int -> Behaviour Int -> Behaviour [Int]
benumFromTo x y = Func2 BhvEnumFromTo x y



jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""
reactive = script ! type_ "text/javascript" ! src "js/reactive.js" $ ""
jsinit = script ! type_ "text/javascript" $ "r_init();"

bhv x = HtmlM $ \s -> ((), ([Behaviour x], s))

page = html $ do
        head $ do
                title "Catopsis"
                jquery; reactive
        body $ do
                jsinit
                let count = SrvVal "count" :: Behaviour Int
                ul $ do
                        let msg = SrvVal "msg" :: Behaviour String
                        let msgs = SrvVal "msgs" :: Behaviour [String]

                        li $ "polozka"
                        --bhv $ Func li $ Func ToHtmlInt count
                        bhv $ Func li $ Func ToHtmlString msg
                        bhv $ Func ToHtmlHtmlList $ bmap li $ bmap ToHtmlString msgs
                        bhv $ Func li $ Func ToHtmlInt $ blength msgs

                bhv $ Func ToHtmlHtmlList $ bmap div $ bmap ToHtmlInt $ benumFromTo (Value 1) count



data RenderState = RenderState { rsUniq :: Int, rsJavascript :: String, rsBhvCache :: [(BhvCacheKey,Int)] }

type RenderMonad a = StateT RenderState (Writer String) a


render :: Html -> String
render (HtmlM xs) = snd $ runWriter $ evalStateT (mapM render' $ fst $ snd $ xs $ HtmlState 1 []) (RenderState 1 "" [])
        where render' :: HtmlStructure -> RenderMonad ()

              render' (Tag tag attrs xs) = do
                      tell $ "<" ++ tag 
                      mapM_ renderAttrs attrs
                      tell ">\n"
                      mapM render' xs
                      tell $ "</" ++ tag ++ ">\n"

              render' (Text text) = tell text
              render' (Behaviour b) = do
                      id <- jsValue b
                      tell $ "<div bhv-id="++show id++"></div>\n"
                      gets (not.null.rsJavascript) >>= flip when renderJavascript
              render' (Placeholder p attrs) = do
                      tell $ "<div placeholder-id=\""++show p++"\""
                      mapM_ renderAttrs attrs
                      tell "></div>\n"

              renderAttrs (AttrBool name) = tell $ ' ':name
              renderAttrs (AttrVal name val) = tell $ " "++name++"=\""++escapeStringHtml val++"\""
              renderAttrs (EventCall name (Event id) value) = do
                      param <- jsValue value
                      tell $ " "++name++"=\"call_event("++show id++","++param++")\""

              renderJavascript = do
                      tell "<script type=\"text/javascript\">\n"
                      tell =<< gets rsJavascript
                      tell "</script>\n"
                      modify $ \s -> s { rsJavascript = [] }



htmlUniq :: HtmlM Int
htmlUniq = do { x <- gets hsUniq; modify $ \s -> s { hsUniq = x+1 }; return x }

renderUniq :: RenderMonad Int
renderUniq = do { x <- gets rsUniq; modify $ \s -> s { rsUniq = x+1 }; return x }

renderPutJS :: String -> RenderMonad ()
renderPutJS code = modify $ \s -> s { rsJavascript = rsJavascript s ++ code ++ "\n" }


renderPutJSFun :: String -> RenderMonad String
renderPutJSFun impl = do
        name <- return.("func_"++).show =<< renderUniq
        renderPutJS $ "function "++name++"(param) {"++impl++"}"
        return name
