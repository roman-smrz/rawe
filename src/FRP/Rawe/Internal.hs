{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

-----------------------------------------------------------------------------
-- |
-- Maintainer  : Roman Smrž <roman.smrz@seznam.cz>
-- Stability   : experimental
--
-- In this module are provided all the core definitions of the library. This
-- interface, however, is considered internal and should not be relied upon.
--
-----------------------------------------------------------------------------

module FRP.Rawe.Internal where


import Prelude hiding (head,div,(.),id,fst,snd,span,curry,uncurry)

import Control.Categorical.Bifunctor
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Category.Monoidal

import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Writer hiding (Product)

import Data.String
import Data.Void

import Text.JSON as J


-- * Basic definitions for HTML

-- | The type representing atrributes of HTML elements
data Attribute = AttrVal String String  -- ^ Key-value pair
               | AttrBool String        -- ^ Boolean attribute


instance BhvValue Attribute where
    bhvValue (AttrVal name value) = do
        (RawJS jn) <- bhvValue name; (RawJS jv) <- bhvValue value
        return.RawJS $ "rawe.cthunk( { AttrVal: ["++jn++","++jv++"] } )"
    bhvValue (AttrBool name) = do
        (RawJS jn) <- bhvValue name
        return.RawJS $ "rawe.cthunk( { AttrBool: ["++jn++"] } )"


-- | Type class used to enable adding attributes to values of both types
-- Html and Html -> Html.
class Attributable b where
    (!) :: b -> Attribute -> b

instance Attributable (HtmlM a) where
    (HtmlM f) ! a = HtmlM $ \s -> let (x, (cs, s')) = f s
                                   in (x, (map (addAttr a) cs, s'))

instance Attributable (HtmlM a -> HtmlM a) where
    f ! a = \html -> HtmlM $ \s -> let (HtmlM g) = f html
                                       (x, (t, s')) = g s
                                    in (x, (map (addAttr a) t, s'))

addAttr :: Attribute -> HtmlStructure -> HtmlStructure
addAttr a (Tag name as content) = Tag name (a:as) content
addAttr a (TagVoid name as) = TagVoid name (a:as)
addAttr _ t = t

instance Attributable (Bhv Html) where
    (!) = primOp2 (!) "add_attr"


data AddAttr = AddAttr Attribute
instance BhvPrim AddAttr Html Html where
    bhvPrim (AddAttr (AttrVal name val)) = do
        jname <- bhvValue name; jval <- bhvValue val
        return ("add_attr", [jname, jval])
    bhvPrim (AddAttr (AttrBool name)) = do
        jname <- bhvValue name; jval <- bhvValue True
        return ("add_attr", [jname, jval])
    unsafeBhvEval (AddAttr a) = (!a)




-- | The structure of HTML tree.
data HtmlStructure = Tag String [Attribute] [HtmlStructure] -- ^ Tag with content
                   | TagVoid String [Attribute]             -- ^ Tag without content
                   | Doctype        -- ^ Doctype declaration
                   | Text String    -- ^ Text node
                   | Behaviour Int  -- ^ Placeholder for HTML behaviour identified by the parameter


-- * The HtmlM monad


-- | The inner state kept in the HtmlM monad.

data HtmlState = HtmlState
    { hsUniq :: Int
        -- ^ Counter for generating unique values

    , hsBehaviours :: [(Int, String, [RawJS])]
        -- ^ List of behaviours with their id, name of the JavaScript
        -- constructor and list of parameters passed to it

    , hsHtmlValues :: [(Int, String, Maybe String)]
        -- ^ List of HTML values with ID of the snipped, the HTML code and
        -- possibly assigned inner value

    , hsHtmlCurrent :: Maybe Int
        -- ^ Set, if we are currently rendering HTML for some other
        -- behaviour

    , hsHtmlBehaviours :: [(Int, Maybe Int)]
        -- ^ Assignments of behaviours to parts of some HTML snippet or the
        -- main page if none given

    , hsHtmlGens :: [(Int, Maybe Int)]
        -- ^ Assignments the generating behaviour to part of some HTML
        -- snipped or the main page

    , hsRecursion :: Int
        -- ^ Level of recursion in executing HtmlM monads.
    }

emptyHtmlState = HtmlState 0 [] [] Nothing [] [] 0


-- | The HtmlM monad.

data HtmlM a = HtmlM (HtmlState -> (a, ([HtmlStructure], HtmlState)))

-- | Type representing pure HTML snippet.

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



instance IsString (Bhv String) where
        fromString = cb

instance IsString Html where
    fromString text = HtmlM $ \s -> ((), ([Text text], s))

-- | Function used to include text node into the HTML page. Forces the type
-- Html insted of arbitrary HtmlM a inferred when using overloaded strings
-- directly.

str :: String -> Html
str = fromString


-- ** Adding behaviours into HtmlM

addBehaviour' :: Int -> String -> [RawJS] -> HtmlM ()
addBehaviour' id name params =
    modify $ \s -> s { hsBehaviours = (id, name, params) : hsBehaviours s }


addBehaviourName :: String -> [RawJS] -> HtmlM (Int, Int)
addBehaviourName name params = do
    bid <- htmlUniq
    r <- gets hsRecursion
    addBehaviour' bid name params
    return (r,bid)


addBehaviour :: BhvFun a b -> HtmlM (Int,Int)
addBehaviour b = do
    -- We need to do this so b does not need to be evaluated for internal
    -- unique counter and mfix may work for mutually recursive behaviours.
    nid <- htmlUniq
    r <- gets hsRecursion

    case b of
         Prim _ hf -> do
             (name, params) <- hf
             addBehaviour' nid name params
             return (r,nid)

         Assigned _ id -> return id
         BhvID -> return (0,0)


-- ** Utility functions

htmlUniq :: HtmlM Int
htmlUniq = do { x <- gets hsUniq; modify $ \s -> s { hsUniq = x+1 }; return x }


htmlLocal :: HtmlM a -> HtmlM a
htmlLocal action = do
    state <- get
    put $ emptyHtmlState { hsRecursion = hsRecursion state + 1, hsUniq = (hsRecursion state + 1) * 1000 }
    result <- action
    put state
    return result


-- * Behaviours

data BhvFun a b = Prim (a -> b) (HtmlM (String, [RawJS]))
                | Assigned (a -> b) (Int, Int)
                | (a ~ b) => BhvID

type Bhv a = BhvFun Void a

type Event a = Bhv (Timed a)


instance Category BhvFun where
    id = BhvID
    BhvID . b = b
    b . BhvID = b
    g . f = prim $ BhvModifier2 (>>>) "compose" f g

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
    x &&& y = prim $ BhvModifier2 (&&&) "product" x y
    fst = primOp fst "fst"
    snd = primOp snd "snd"

instance CCC BhvFun where
    type Exp BhvFun = (->)
    apply = primOp apply "apply"
    curry = prim . BhvModifier curry "curry"
    uncurry = prim . BhvModifier uncurry "uncurry"


class BhvValue a where
    bhvValue :: a -> HtmlM RawJS

instance BhvValue (BhvFun a b) where
    bhvValue f = do (r,i) <- addBehaviour f
                    return $ RawJS $ "rawe.cthunk(r_bhv_fun_"++show r++"["++show i++"])"

instance BhvValue () where bhvValue () = return "rawe.cthunk([])"
instance BhvValue Int where bhvValue x = return.RawJS $ "rawe.cthunk("++show x++")"
instance BhvValue Float where bhvValue x = return.RawJS $ "rawe.cthunk("++show x++")"
instance BhvValue Bool where bhvValue x = return $ if x then "rawe.cthunk(true)" else "rawe.cthunk(false)"

instance BhvValue Char where
    bhvValue x = return.RawJS $ "rawe.cthunk("++show x++")"

instance (BhvValue a, BhvValue b) => BhvValue (a, b) where
    bhvValue (x,y) = do
        (RawJS x') <- bhvValue x; (RawJS y') <- bhvValue y
        return.RawJS $ "rawe.cthunk(["++x'++", "++y'++"])"

instance (BhvValue a) => BhvValue (Maybe a) where
    bhvValue (Just x) = do (RawJS jx) <- bhvValue x
                           return.RawJS $ "rawe.cthunk({Just:"++jx++"})"
    bhvValue Nothing  = return.RawJS $ "rawe.cthunk({Nothing:null})"

instance BhvValue a => BhvValue [a] where
    bhvValue [] = return.RawJS $ "rawe.cthunk({nil:[]})"
    bhvValue (x:xs) = do RawJS jx <- bhvValue x
                         RawJS jxs <- bhvValue xs
                         return.RawJS $ "rawe.cthunk({cons:["++jx++","++jxs++"]})"





class BhvPrim f a b | f -> a b where
    bhvPrim :: f -> HtmlM (String, [RawJS])
    unsafeBhvEval :: f -> a -> b

prim :: (BhvPrim f a b) => f -> BhvFun a b
prim f = Prim (unsafeBhvEval f) (bhvPrim f)


data BhvPrimFunc a b = BhvPrimFunc (a -> b) String
instance BhvPrim (BhvPrimFunc a b) a b where
    bhvPrim (BhvPrimFunc _ name) = return (name, [])
    unsafeBhvEval (BhvPrimFunc f _) = f


data BhvModifier a b c d = BhvModifier ((a -> b) -> (c -> d)) String (BhvFun a b)
instance BhvPrim (BhvModifier a b c d) c d where
    bhvPrim (BhvModifier _ name x) = do jx <- bhvValue x
                                        return (name, [jx])
    unsafeBhvEval (BhvModifier f _ b) = f (unsafeBfEval b)


data BhvModifier2 a b c d e f = BhvModifier2 ((a -> b) -> (c -> d) -> (e -> f)) String (BhvFun a b) (BhvFun c d)
instance BhvPrim (BhvModifier2 a b c d e f) e f where
    bhvPrim (BhvModifier2 _ name x y) = do jx <- bhvValue x; jy <- bhvValue y
                                           return (name, [jx, jy])
    unsafeBhvEval (BhvModifier2 f _ b1 b2) = f (unsafeBfEval b1) (unsafeBfEval b2)


data BhvConst a b = (BhvValue b) => BhvConst b
instance BhvPrim (BhvConst a b) a b where
    bhvPrim (BhvConst x) = do jx <- bhvValue x
                              return ("const", [jx])
    unsafeBhvEval (BhvConst x) = const x


cb :: (BhvValue a) => a -> BhvFun b a
cb = prim . BhvConst


primOp :: (a -> b) -> String -> BhvFun a b
primOp f = prim . BhvPrimFunc f

primOp0 :: a -> String -> Bhv a
primOp0 x name = primOp (const x) name

primOp1 :: BhvValueFun a a' => (a' -> b) -> String -> a -> Bhv b
primOp1 f name a = primOp f name . (cbf a)

primOp2 :: (BhvValueFun a a', BhvValueFun b b') => (a' -> b' -> c) -> String -> a -> b -> Bhv c
primOp2 f name a b = primOp (uncurry f) name . (cbf a &&& cbf b)

primOp3 :: (BhvValueFun a a', BhvValueFun b b', BhvValueFun c c') => (a' -> b' -> c' -> d) -> String -> a -> b -> c -> Bhv d
primOp3 f name a b c = primOp (uncurry $ \x -> uncurry (f x)) name . (cbf a &&& cbf b &&& cbf c)

primOp4 :: (BhvValueFun a a', BhvValueFun b b', BhvValueFun c c', BhvValueFun d d') =>
    (a' -> b' -> c' -> d' -> e) -> String -> a -> b -> c -> d -> Bhv e
primOp4 f name a b c d = primOp (uncurry $ \x -> uncurry $ \y -> uncurry (f x y))
    name . (cbf a &&& cbf b &&& cbf c &&& cbf d)

primOp5 :: (BhvValueFun a a', BhvValueFun b b', BhvValueFun c c', BhvValueFun d d', BhvValueFun e e') =>
    (a' -> b' -> c' -> d' -> e' -> f) -> String -> a -> b -> c -> d -> e -> Bhv f
primOp5 f name a b c d e = primOp (uncurry $ \x -> uncurry $ \y -> uncurry $ \z -> uncurry (f x y z))
    name . (cbf a &&& cbf b &&& cbf c &&& cbf d &&& cbf e)


-- ** Lifting of functions

instance (BhvValue b) => BhvValue (Bhv a -> b) where
    bhvValue = bhvValueCommon bhvValue $ \r iid ->
        [ "var r_bhv_fun_"++show r++" = {};"
        , "r_bhv_fun_"++show r++"["++show iid++"] = param.get();"
        ]


class BhvValueFun a b | a -> b where
    bhvValueFun :: a -> HtmlM RawJS
    bhvValueFunEval :: a -> b
    cbf :: a -> Bhv b

instance BhvValueFun (Bhv a) a where
    bhvValueFun x = do RawJS jx <- bhvValue x
                       return $ RawJS (jx ++ ".get().compute()")
    bhvValueFunEval x = (unsafeBfEval x) void
    cbf = id

instance BhvValueFun Attribute Attribute where
    bhvValueFun = bhvValue
    bhvValueFunEval = id
    cbf = cb

instance BhvValueFun b b' => BhvValueFun (Bhv a -> b) (a -> b') where
    bhvValueFun = bhvValueCommon bhvValueFun $ \r iid ->
        [ "var r_bhv_fun_"++show r++" = {};"
        , "r_bhv_fun_"++show r++"["++show iid++"] = new rawe.BhvFun();"
        , "rawe.prim.const.call(r_bhv_fun_"++show r++"["++show iid++"], param);"
        ]
    bhvValueFunEval f = \x -> bhvValueFunEval (f (prim $ BhvConstEval x))
    cbf = prim . BhvConstFun


data BhvConstFun a b b' = (BhvValueFun b b') => BhvConstFun b
instance BhvPrim (BhvConstFun a b b') a b' where
    bhvPrim (BhvConstFun x) = do jx <- bhvValueFun x
                                 return ("const", [jx])
    unsafeBhvEval (BhvConstFun x) = const (bhvValueFunEval x)


bhvValueCommon bv begin f = htmlLocal $ do
    iid <- htmlUniq
    r <- gets hsRecursion
    RawJS result <- bv $ f (Assigned (error "eval: bhvValueCommon") (r,iid))

    bs <- gets $ reverse.hsBehaviours
    hs <- gets $ reverse.hsHtmlValues
    hbs <- gets $ reverse.hsHtmlBehaviours
    hgs <- gets $ reverse.hsHtmlGens

    return $ RawJS $ init $ unlines $
        ("rawe.cthunk(function(param) {":) $ (++[replicate (r-1) '\t' ++ "})"]) $
        map (replicate r '\t' ++) $ (begin r iid ++) $
        concat
        [ flip map bs $ \(id, func, params) ->
            "r_bhv_fun_"++show r++"["++show id++"] = new rawe.BhvFun("++show id++");"

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
                jfunc = "rawe.prim."++func
                jparams = concatMap ((',':).unRawJS) params
             in jfunc++".call(r_bhv_fun_"++show r++"["++jid++"]"++jparams++");"

        , flip map hbs $ \(i, _) ->
            "r_bhv_fun_"++show r++"["++show i++"].invalidate();"
        ]
        ++
        [ "rawe.init(r_bhv_fun_"++show r++");"
        , "return "++result++";"
        ]





--------------------------------------------------------------------------------
-- ** Evaluating to Haskell functions

unsafeBfEval :: BhvFun a b -> a -> b
unsafeBfEval (Prim f _) = f
unsafeBfEval (Assigned f _) = f
unsafeBfEval (BhvID) = id


instance BhvValue Void where
    bhvValue _ = return "null"

void :: Void
void = error "void"



class BhvEval a b | a -> b where
    unsafeEval :: a -> b
    unsafeUneval :: b -> a

instance BhvValue a => BhvEval (Bhv a) a where
    unsafeEval = flip unsafeBfEval void
    unsafeUneval = prim . BhvConstEval

instance (BhvEval a a', BhvEval b b') => BhvEval (a -> b) (a' -> b') where
    unsafeEval f = unsafeEval . f . unsafeUneval
    unsafeUneval f = unsafeUneval . f . unsafeEval

data BhvConstEval a b = BhvConstEval b
instance BhvPrim (BhvConstEval a b) a b where
    bhvPrim _ = error "BhvConstEval: bhvPrim"
    unsafeBhvEval (BhvConstEval x) = const x



--------------------------------------------------------------------------------
-- * Rendering page


render :: Html -> IO String
render html = let (HtmlM f) = renderH html
                  ((result, ()), _) = f (emptyHtmlState { hsUniq = 1, hsBehaviours = [(0, "id", [])] } )
               in return result


renderH :: HtmlM a -> HtmlM (String, a)
renderH (HtmlM f) = do
    (x, (xs, s')) <- gets f
    put s'
    return (execWriter (mapM_ render' xs), x)

    where render' :: HtmlStructure -> Writer String ()

          render' (Tag tag attrs xs) = do
              tell $ "<" ++ tag
              mapM_ renderAttrs attrs
              tell ">\n"
              mapM render' xs
              tell $ "</" ++ tag ++ ">\n"

          render' (TagVoid tag attrs) = do
              tell $ "<" ++ tag
              mapM_ renderAttrs attrs
              tell ">\n"

          render' (Doctype) = tell "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n"

          render' (Text text) = tell text

          render' (Behaviour id) = do
              tell $ "<div bhv-id="++show id++"></div>\n"

          renderAttrs (AttrBool name) = tell $ ' ':name
          renderAttrs (AttrVal name val) = tell $ " "++name++"=\""++escapeStringHtml val++"\""


escapeStringHtml = (>>=helper)
    where helper '"' = "&quot;"
          helper '&' = "&amp;"
          helper '<' = "&lt;"
          helper '>' = "&gt;"
          helper c = [c]



-- * Other definitions


-- ** Timed type, constructors, destructor and instances

data Time

data Timed a = NotYet | OnTime Time a


notYet :: Bhv (Timed a)
notYet = primOp0 NotYet "not_yet"

onTime :: Bhv Time -> Bhv a -> Bhv (Timed a)
onTime = primOp2 OnTime "on_time"

timed :: Bhv b -> (Bhv Time -> Bhv a -> Bhv b) -> Bhv (Timed a) -> Bhv b
timed = primOp3 (\ny ot x -> case x of NotYet -> ny; OnTime t y -> ot t y) "timed"

data TimedFold a b = TimedFold (Bhv Time -> Bhv a -> Bhv b -> Bhv a) (Bhv a) (Bhv (Timed b))
instance BhvPrim (TimedFold a b) Void a where
    bhvPrim (TimedFold step def ev) = do
        jstep <- bhvValue $ cbf step
        jdef <- bhvValue def
        jev <- bhvValue ev
        return ("timed_fold", [jstep, jdef, jev])
    unsafeBhvEval (TimedFold _ def _) = unsafeBfEval def

timedFold :: (Bhv Time -> Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv (Timed b) -> Bhv a
timedFold f x = prim . TimedFold f x



-- ** JSString constructor and destructor

toJSString :: Bhv String -> Bhv JSString
toJSString = primOp1 J.toJSString "to_js_string"

fromJSString :: Bhv JSString -> Bhv String
fromJSString = primOp1 J.fromJSString "from_js_string"

instance BhvValue JSString where
    bhvValue str = return.RawJS $ "rawe.cthunk('"++escapeStringJS (J.fromJSString str)++"')"



-- ** Misc primitives

bhvWrap :: BhvFun a (Bhv a)
bhvWrap = primOp (prim . BhvConstEval) "bhv_wrap"

bhvUnwrap :: BhvFun (Bhv a) a
bhvUnwrap = primOp (flip unsafeBfEval void) "bhv_unwrap"

bhvToHask :: BhvFun a b -> (Bhv a -> Bhv b)
bhvToHask = (.)

haskToBhv :: (Bhv a -> Bhv b) -> BhvFun a b
haskToBhv f = bhvUnwrap . apply . (cb f &&& bhvWrap)



newtype RawJS = RawJS { unRawJS :: String }
instance IsString RawJS where fromString = RawJS

escapeStringJS = (>>=helper)
    where helper '\n' = "\\n"
          helper c | c `elem` "'\"\\" = '\\':c:[]
                   | otherwise        = [c]
