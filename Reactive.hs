{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs,
  FunctionalDependencies, OverloadedStrings,
  TypeSynonymInstances, NoMonomorphismRestriction #-}

module Reactive where

import Prelude hiding (head,div,(.),id)

import Control.Category

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

instance JSValue () where jsValue () = return "null"
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


data BehaviourFun a b = forall f. (JSValue a, JSValue b, JSFunc f a b) => Prim f
                      | forall c. (JSValue a, JSValue b, JSValue c) => Compound (BehaviourFun c b) (BehaviourFun a c)
                      | (JSValue a, JSValue b) => SrvVal String
                      | (JSValue a, JSValue b) => Until (BehaviourFun a b) (Event (BehaviourFun a b))
                      | (a ~ b) => BhvID

type Behaviour a = BehaviourFun () a

instance Category BehaviourFun where
        id = BhvID
        BhvID . b = b
        b . BhvID = b
        x@(Prim _) . y@(Prim _) = Compound x y
        x@(Prim _) . y@(Compound _ _) = Compound x y
        x@(Prim _) . y@(SrvVal _) = Compound x y
        x@(Prim _) . y@(Until _ _) = Compound x y
        x@(Compound _ _) . y@(Prim _) = Compound x y
        x@(Compound _ _) . y@(Compound _ _) = Compound x y
        x@(Compound _ _) . y@(SrvVal _) = Compound x y
        x@(Compound _ _) . y@(Until _ _) = Compound x y
        x@(SrvVal _) . y@(Prim _) = Compound x y
        x@(SrvVal _) . y@(Compound _ _) = Compound x y
        x@(SrvVal _) . y@(SrvVal _) = Compound x y
        x@(SrvVal _) . y@(Until _ _) = Compound x y
        x@(Until _ _) . y@(Prim _) = Compound x y
        x@(Until _ _) . y@(Compound _ _) = Compound x y
        x@(Until _ _) . y@(SrvVal _) = Compound x y
        x@(Until _ _) . y@(Until _ _) = Compound x y



($$) :: BehaviourFun a (BehaviourFun b c) -> Behaviour a -> BehaviourFun b c
f $$ x = undefined

(&&&) :: BehaviourFun a b -> BehaviourFun a c -> BehaviourFun a (b,c)
x@(Prim _) &&& y@(Prim _) = Prim $ BhvProduct x y
x@(Prim _) &&& y@(Compound _ _) = Prim $ BhvProduct x y
x@(Prim _) &&& y@(SrvVal _) = Prim $ BhvProduct x y
x@(Compound _ _) &&& y@(Prim _) = Prim $ BhvProduct x y
x@(Compound _ _) &&& y@(Compound _ _) = Prim $ BhvProduct x y
x@(Compound _ _) &&& y@(SrvVal _) = Prim $ BhvProduct x y
x@(SrvVal _) &&& y@(Prim _) = Prim $ BhvProduct x y
x@(SrvVal _) &&& y@(Compound _ _) = Prim $ BhvProduct x y
x@(SrvVal _) &&& y@(SrvVal _) = Prim $ BhvProduct x y
infixr 3 &&&



data BhvProduct a b c = BhvProduct (BehaviourFun a b) (BehaviourFun a c)
instance (JSValue a, JSValue b, JSValue c) => JSFunc (BhvProduct a b c) a (b,c) where
        jsFunc (BhvProduct x y) = do
                jx <- jsFunc x; jy <- jsFunc y
                return $ "r_product("++jx++","++jy++")"



instance (JSValue a, JSValue b) => JSFunc (BehaviourFun a b) a b where
        jsFunc (Prim f) = jsFunc f

        jsFunc (Compound f g) = do
                jf <- jsFunc f; jg <- jsFunc g
                return $ "r_compose("++jf++","++jg++")"

        jsFunc (SrvVal name) = do
                renderPutJSFun $ "return r_srv_call("++show name++");"

        jsFunc (Until f (Event ev)) = do
                jf <- jsFunc f
                renderPutJSFun $ "return r_until("++jf++", "++show ev++");"

instance (JSValue a, JSValue b) => JSValue (BehaviourFun a b) where jsValue = jsFunc



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
addAttr a (Behaviour b) = Behaviour $ Prim (AddAttr a) . b
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
bmap :: (JSValue a, JSValue b, JSFunc f a b) => f -> BehaviourFun [a] [b]
bmap f = Prim (BhvMap f)

data BhvLength a = (JSValue a) => BhvLength
instance JSFunc (BhvLength a) [a] Int where
        jsFunc BhvLength = renderPutJSFun $ "return param.length;"
blength :: (JSValue a) => BehaviourFun [a] Int
blength = Prim BhvLength

data BhvEnumFromTo = BhvEnumFromTo
instance JSFunc BhvEnumFromTo (Int,Int) [Int] where
        jsFunc BhvEnumFromTo = return "r_prim_enumFromTo"
benumFromTo :: BehaviourFun (Int,Int) [Int]
benumFromTo = Prim BhvEnumFromTo

data BhvConst a b = (JSValue b) => BhvConst b
instance (JSFunc (BhvConst a b) a b) where
        jsFunc (BhvConst x) = do val <- jsValue x
                                 renderPutJSFun $ "return "++val++";"
instance (Num b) => Eq (BehaviourFun a b) where
        _ == _ = True
instance (Num b) => Show (BehaviourFun a b) where
        show _ = ""
instance (JSValue a, JSValue b, Num b) => Num (BehaviourFun a b) where
        fromInteger = Prim . BhvConst . fromIntegral




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
                        bhv $ Prim li . Prim ToHtmlInt . count
                        bhv $ Prim li . Prim ToHtmlString . msg
                        bhv $ Prim ToHtmlHtmlList . bmap li . bmap ToHtmlString . msgs
                        bhv $ Prim li . Prim ToHtmlInt . blength . msgs

                bhv $ Prim ToHtmlHtmlList . bmap div . bmap ToHtmlInt . benumFromTo . (1 &&& count)



data RenderState = RenderState { rsUniq :: Int, rsJavascript :: String }

type RenderMonad a = StateT RenderState (Writer String) a


render :: Html -> String
render (HtmlM xs) = snd $ runWriter $ evalStateT (mapM render' $ fst $ snd $ xs $ HtmlState 1 []) (RenderState 1 "")
        where render' :: HtmlStructure -> RenderMonad ()

              render' (Tag tag attrs xs) = do
                      tell $ "<" ++ tag 
                      mapM_ renderAttrs attrs
                      tell ">\n"
                      mapM render' xs
                      tell $ "</" ++ tag ++ ">\n"

              render' (Text text) = tell text

              render' (Behaviour b) = do
                      id <- renderUniq
                      f <- jsFunc b
                      tell $ "<div bhv-id="++show id++"></div>\n"
                      renderPutJS $ "r_bhv_func["++show id++"] = "++f++";"
                      renderJavascript

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
