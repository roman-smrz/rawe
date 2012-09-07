{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Internal.hs: definition of core types and functions
-- as part of rawe - ReActive Web Framework
--
-- Copyright 2011 Roman Smrž <roman.smrz@seznam.cz>, see file LICENSE

-----------------------------------------------------------------------------
-- |
-- Maintainer  : Roman Smrž <roman.smrz@seznam.cz>
-- Stability   : experimental
--
-- In this module are provided all the core definitions of the library. This
-- interface, however, is considered internal and should not be relied upon.
--
-- Various functions defined here are re-exported from other modules: Rawe.hs
-- for basic types and rendering; Prelude.hs mainly for definitions of
-- equivalents of standard functions, but also contains various methods
-- handling events and some utility functions; Html.hs contains mainly
-- combinators for constructing pages.
--
-----------------------------------------------------------------------------

module FRP.Rawe.Internal where


import Prelude hiding (head,div,(.),id,fst,snd,span,curry,uncurry)

import Control.Applicative
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

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Data.String
import Data.Void

import Text.JSON as J


-- * Basic definitions for HTML

-- | The type representing attributes of HTML elements
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
    { hsUniq :: IntMap Int
        -- ^ Counter for generating unique values

    , hsBehaviours :: IntMap [(Int, String, [RawJS])]
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

    , hsRecNedded :: Int
        -- ^ Recursion level needed for used behaviours

    , hsInFix :: Bool
        -- ^ Indicates whether we are inside an mfix call
    }

emptyHtmlState = HtmlState (IM.singleton 0 0) (IM.singleton 0 []) [] Nothing [] [] 0 0 False


-- | The HtmlM monad.

data HtmlM a = HtmlM (HtmlState -> (a, ([HtmlStructure], HtmlState)))

-- | Type representing pure HTML snippet.

type Html = HtmlM ()


instance Functor HtmlM where
    fmap f (HtmlM hf) = HtmlM $ \s -> (\(x, hs)->(f x, hs)) (hf s)

instance Applicative HtmlM where
    pure = return
    (<*>) = ap

instance Monad HtmlM where
    return x = HtmlM $ \s -> (x, ([], s))
    (HtmlM f) >>= g = HtmlM $ \s -> let (x, (cs, s')) = f s
                                        (HtmlM g') = g x
                                        (y, (ds, s'')) = g' s'
                                     in (y, (cs++ds, s''))

instance MonadFix HtmlM where
    mfix f = HtmlM $ \s -> let (HtmlM f') = f x
                               (x, (t, s')) = f' ( s { hsInFix = True } )
                            in (x, (t, s' { hsInFix = False } ))

instance MonadState HtmlState HtmlM where
    get = HtmlM $ \s -> (s, ([], s))
    put s = HtmlM $ \_ -> ((), ([], s))



instance IsString (Bhv String) where
        fromString = cb

instance IsString Html where
    fromString text = HtmlM $ \s -> ((), ([Text text], s))

-- | Function used to include text node into the HTML page. Forces the type
-- Html instead of arbitrary HtmlM a inferred when using overloaded strings
-- directly.

str :: String -> Html
str = fromString


-- ** Adding behaviours into HtmlM

-- | Adds a behaviour to the list of behaviours the state of HtmlM.

addBehaviour
    :: Int      -- ^ Level of recursion
    -> Int      -- ^ ID used for this behaviour
    -> String   -- ^ Name of the initialization function
    -> [RawJS]  -- ^ Parameters for the initialization function
    -> HtmlM ()
addBehaviour r id name params =
    modify $ \s -> s
        { hsBehaviours = IM.adjust ((id, name, params):) r (hsBehaviours s)
        , hsRecNedded = max r (hsRecNedded s)
        }


-- | Similar to 'addBehaviour', but generates a unique ID.

addBehaviourName :: String -> [RawJS] -> HtmlM (Int, Int)
addBehaviourName name params = do
    bid <- htmlUniq
    r <- gets hsRecursion
    addBehaviour r bid name params
    modify $ \s -> s { hsRecNedded = max r (hsRecNedded s) }
    return (r,bid)


-- | Returns an ID of given behaviour in the current HtmlM monad. It may or may
-- not add a new one to the global list of behaviours.

assignBehaviour :: BhvFun a b -> HtmlM (Int,Int)
assignBehaviour b = do
    -- We need to do this so b does not need to be evaluated for internal
    -- unique counter and mfix may work for mutually recursive behaviours.
    nids <- htmlUniqs
    r <- gets hsRecursion

    let checkExisting getCode = do
            origNeed <- gets hsRecNedded
            modify $ \s -> s { hsRecNedded = 0 }
            (name, params) <- getCode
            need <- gets hsRecNedded
            modify $ \s -> s { hsRecNedded = max need origNeed }
            let rneed = min r need

            inFix <- gets hsInFix
            let nid = nids rneed
            if inFix
               then do addBehaviour rneed nid name params
                       return (rneed,nid)

               else do mbid <- return . find (\(_, n, p) -> (n,p)==(name,params)) . (IM.!rneed) =<< gets hsBehaviours
                       case mbid of
                            Just (id, _, _) -> return (rneed,id)
                            Nothing -> do addBehaviour rneed nid name params
                                          return (rneed,nid)

    case b of
         Prim _ hf -> checkExisting hf
         Composed _ _ -> checkExisting $ (,) "compose" <$> composedList b

         Assigned _ (r,id) -> do
             modify $ \s -> s { hsRecNedded = max r (hsRecNedded s) }
             return (r,id)

         BhvID -> return (0,0)



composedList :: BhvFun a b -> HtmlM [RawJS]
composedList (Composed x y) = (++) <$> composedList x <*> composedList y
composedList x = (:[]) <$> bhvValue x

-- ** Utility functions

-- | Generates a unique integer.

htmlUniq :: HtmlM Int
htmlUniq = do
    r <- gets hsRecursion
    us <- gets hsUniq
    let x = us IM.! r
    modify $ \s -> s { hsUniq = IM.insert r (x+1) us }
    return x


-- | Generates unique integer for every recursion level

htmlUniqs :: HtmlM (Int -> Int)
htmlUniqs = do
    us <- gets hsUniq
    modify $ \s -> s { hsUniq = IM.map (+1) us }
    return (us IM.!)

-- | Creates an environment for a "recursive" call used to list functions
-- between behaviours. Increments the recursion counter, and resets the unique
-- counter (for somewhat easier debugging is the counter initialized to 1000 *
-- <recursion level>), then executes given action and restores original state.

htmlLocal :: HtmlM a -> HtmlM a
htmlLocal action = do
    state <- get
    let newrec = hsRecursion state + 1
    put $ emptyHtmlState
            { hsRecursion = newrec
            , hsUniq = IM.insert newrec (1000*newrec) (hsUniq state)
            , hsBehaviours = IM.insert newrec [] (hsBehaviours state)
            , hsInFix = hsInFix state
            }
    result <- action
    state' <- get
    put $ state
            { hsBehaviours = IM.delete newrec (hsBehaviours state')
            , hsUniq = IM.delete newrec (hsUniq state')
            , hsRecNedded = max (hsRecNedded state) (hsRecNedded state')
            }
    return result


-- * Behaviours

-- | The representation of behaviour functions

data BhvFun a b = Prim (a -> b) (HtmlM (String, [RawJS]))
                -- ^ Primitive function, keeps the function for evaluating and
                -- an action generating name of parameters of the primitive

                | Assigned (a -> b) (Int, Int)
                -- ^ Behaviour function assigned in some instance of HtmlM monad,
                -- keeps function for evaluating and the IDs.

                | forall c. Composed (BhvFun a c) (BhvFun c b)
                -- ^ Composition of two behaviour functions; multiple compositions
                -- are flattened when generating JavaScript code.

                | (a ~ b) => BhvID
                -- ^ An identity function.

-- | The type representing behaviours.

type Bhv a = BhvFun Void a

-- | The type representing events.

type Event a = Bhv (Timed a)


-- Straightforward instances of various type classes from the package
-- categories.

instance Category BhvFun where
    id = BhvID
    BhvID . b = b
    b . BhvID = b
    g . f = Composed f g

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


-- | 'BhvValue' is the type class representing the types which can be converted
-- to JavaScript and thus lifted into behaviours.

class BhvValue a where
    bhvValue :: a -> HtmlM RawJS
    -- ^ The expression is evaluated inside an HtmlM monad to allow
    -- representing expressing, which depend on the inner state of HtmlM


-- Follow instances for basic types:

instance BhvValue (BhvFun a b) where
    bhvValue f = do (r,i) <- assignBehaviour f
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

instance (BhvValue a, BhvValue b) => BhvValue (Either a b) where
    bhvValue (Left x) = do RawJS jx <- bhvValue x
                           return.RawJS $ "rawe.cthunk({Left:"++jx++"})"
    bhvValue (Right x) = do RawJS jx <- bhvValue x
                            return.RawJS $ "rawe.cthunk({Right:"++jx++"})"


-- | 'BhvPrim' was originally used in the definition of 'BhvFun' in the
-- constructor 'Prim' as a type class constraint for an existential type of the
-- function; for this reason, there are various data structures instantiating
-- this type class instead of calling the 'Prim' constructor directly.

class BhvPrim f a b | f -> a b where
    bhvPrim :: f -> HtmlM (String, [RawJS])
    unsafeBhvEval :: f -> a -> b

prim :: (BhvPrim f a b) => f -> BhvFun a b
prim f = Prim (unsafeBhvEval f) (bhvPrim f)


-- | Behaviour function without any parameter for the initialization function.

data BhvPrimFunc a b = BhvPrimFunc (a -> b) String
instance BhvPrim (BhvPrimFunc a b) a b where
    bhvPrim (BhvPrimFunc _ name) = return (name, [])
    unsafeBhvEval (BhvPrimFunc f _) = f


-- | Behaviour function that modifies another and that is passed to the
-- initializer as a parameter.

data BhvModifier a b c d = BhvModifier ((a -> b) -> (c -> d)) String (BhvFun a b)
instance BhvPrim (BhvModifier a b c d) c d where
    bhvPrim (BhvModifier _ name x) = do jx <- bhvValue x
                                        return (name, [jx])
    unsafeBhvEval (BhvModifier f _ b) = f (unsafeBfEval b)


-- | Similar to BhvModifier, but has two parameter for the initialization function.

data BhvModifier2 a b c d e f = BhvModifier2 ((a -> b) -> (c -> d) -> (e -> f)) String (BhvFun a b) (BhvFun c d)
instance BhvPrim (BhvModifier2 a b c d e f) e f where
    bhvPrim (BhvModifier2 _ name x y) = do jx <- bhvValue x; jy <- bhvValue y
                                           return (name, [jx, jy])
    unsafeBhvEval (BhvModifier2 f _ b1 b2) = f (unsafeBfEval b1) (unsafeBfEval b2)


-- | Constant behaviour function.

data BhvConst a b = (BhvValue b) => BhvConst b
instance BhvPrim (BhvConst a b) a b where
    bhvPrim (BhvConst x) = do jx <- bhvValue x
                              return ("cb", [jx])
    unsafeBhvEval (BhvConst x) = const x

cb :: (BhvValue a) => a -> BhvFun b a
cb = prim . BhvConst


-- Following are various utility function for constructing primitive operators,
-- which do not require parameter for their initialization function. Those are
-- provided for function of up to five parameters.

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

-- instance of BhvValue for function types

instance (BhvValue b) => BhvValue (Bhv a -> b) where
    bhvValue = bhvValueCommon bhvValue $ \r iid ->
        [ "var r_bhv_fun_"++show r++" = {};"
        , "r_bhv_fun_"++show r++"["++show iid++"] = param.get();"
        ]


-- | This class is somewhat similar to the 'BhvValue'; it is used to lift
-- functions between behaviours to a behaviour of function on values (instead
-- of behaviour of functions between behaviours as the 'BhvValue' + 'cb'
-- combination would do) in order to avoid some of the wrapping and unwrapping.

class BhvValueFun a b | a -> b where
    bhvValueFun :: a -> HtmlM RawJS
    bhvValueFunEval :: a -> b
    cbf :: a -> Bhv b

instance BhvValueFun (Bhv a) a where
    bhvValueFun = bhvValue
    bhvValueFunEval x = (unsafeBfEval x) novalue
    cbf = id

instance BhvValueFun Attribute Attribute where
    bhvValueFun = bhvValue
    bhvValueFunEval = id
    cbf = cb

instance BhvValueFun b b' => BhvValueFun (Bhv a -> b) (a -> b') where
    bhvValueFun = bhvValueCommon bhvValueFun $ \r iid ->
        [ "var r_bhv_fun_"++show r++" = {};"
        , "r_bhv_fun_"++show r++"["++show iid++"] = param.get();"
        ]
    bhvValueFunEval f = \x -> bhvValueFunEval (f (prim $ BhvConstEval x))
    cbf = prim . BhvConstFun


data BhvConstFun a b b' = (BhvValueFun b b') => BhvConstFun b
instance BhvPrim (BhvConstFun a b b') a b' where
    bhvPrim (BhvConstFun x) = do jx <- bhvValueFun x
                                 return ("cbf", [jx])
    unsafeBhvEval (BhvConstFun x) = const (bhvValueFunEval x)


-- | The actual implementation of lifting function between behaviours.

bhvValueCommon bv begin f = htmlLocal $ do
    -- First we create a new behaviour
    iid <- htmlUniq
    r <- gets hsRecursion

    -- Which is then passed to the function f, thus giving a value, which we
    -- can then convert to JavaScript representation
    RawJS result <- bv $ f (Assigned (error "eval: bhvValueCommon") (r,iid))

    -- This forced to evaluate the result of f and added all the necessary
    -- behaviour function to our local state, so now, we take them and make
    -- proper JavaScript code from them:

    bs <- gets $ reverse.(IM.!r).hsBehaviours
    hs <- gets $ reverse.hsHtmlValues
    hbs <- gets $ reverse.hsHtmlBehaviours
    hgs <- gets $ reverse.hsHtmlGens

    return $ RawJS $ init $ unlines $
        ("rawe.cthunk(function(param) {":) $ (++[replicate (r-1) '\t' ++ "})"]) $

        -- the begin is implemented in the function, which called us, because
        -- the code they need here differs a bit. However, in any case, they
        -- has to initialize the behaviour we created in the beginning using
        -- the JavaScript formal parameter in order to make the whole thing
        -- work.

        map (replicate r '\t' ++) $ (begin r iid ++) $
        concat

        -- First we create all the objects
        [ flip map bs $ \(id, _, _) ->
            "r_bhv_fun_"++show r++"["++show id++"] = new rawe.BhvFun("++show id++");"

        -- Define HTML snippets, which are dynamically created
        , flip map hs $ \(i, h, mi) ->
            "var r_html_"++show i++" = $('"++escapeStringJS h++"')" ++
            case mi of { Nothing -> ""; Just inner -> ".prop('rawe_html_inner', "++inner++")" }
            ++ ";"

        -- Assign to behaviour those HTML snippets for which they are responsible
        , flip map hbs $ \(i, mv) ->
            let var = case mv of Nothing -> "$('body')"
                                 Just v  -> "r_html_"++show v
             in "r_bhv_fun_"++show r++"["++show i++"].html = "++var++".find('*[bhv-id="++show i++"]');"

        -- Assign to behaviurs HTML elements, which they use to generate values / events.
        , flip map hgs $ \(i, mv) ->
            let var = case mv of Nothing -> "$('body')"
                                 Just v  -> "r_html_"++show v
             in "r_bhv_fun_"++show r++"["++show i++"].gen = "++var++".find2('*[bhv-gen="++show i++"]');"

        -- And finally call all the initialization functions.
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

-- | In some situation, it may be desirable to actually execute the code with
-- behaviours directly in the Haskell program itself. For this purpose, we have
-- this evaluation function. The behaviour function is executed as if in the
-- time at the beginning; no events occurred, no input was entered and so on.
-- The unsafe- prefix is used, because it loses the requirement that the value
-- of type 'a' is represented in JavaScript and thus, if we generate some
-- values of type 'Bhv', those may fail, if we try to pass them into the
-- JavaScript world; the simplest example is the function
--
-- > unsafeBfEval bhvWrap :: a -> Bhv a
--
-- without the type class constraint on the type 'a' seen in the function 'cb',
-- so it can not work correctly. Similar issue arises when evaluating
-- behaviours passed through 'HtmlM'.

unsafeBfEval :: BhvFun a b -> a -> b
unsafeBfEval (Prim f _) = f
unsafeBfEval (Assigned f _) = f
unsafeBfEval (Composed f g) = unsafeBfEval g . unsafeBfEval f
unsafeBfEval (BhvID) = id


instance BhvValue Void where
    bhvValue _ = return "null"

novalue :: Void
novalue = error "novalue"



class BhvEval a b | a -> b where

    -- | This is similar to the 'unsafeBfEval', but since we usually do not work
    -- directly with behaviour functions, but rather with the more practical
    -- functions between behaviours, this takes care of more general ''unwrapping''.
    -- Given definition
    --
    -- > length :: Bhv [a] -> Bhv Int
    -- > length = bfix $ \len -> list 0 (\_ xs -> 1 + len xs)
    --
    -- We may use
    --
    -- > unsafeEval length "abcd"
    --
    -- to get 4 directly in Haskell.

    unsafeEval :: a -> b
    unsafeUneval :: b -> a

instance BhvValue a => BhvEval (Bhv a) a where
    unsafeEval = flip unsafeBfEval novalue
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


-- | This function renders values of type 'Html' into a string representation.
-- The IO type is used, because that form may reveal differences between
-- otherwise semantically equivalent expressions (which are thus treated as
-- equal) and we do not want to break referential transparency.

render :: Html -> IO String
render html = let (HtmlM f) = renderH html
                  ((result, ()), _) = f (emptyHtmlState { hsUniq = IM.singleton 0 1, hsBehaviours = IM.singleton 0 [(0, "id", [])] } )
               in return result


-- | Variant of 'render', which evaluates itself in another instance of HtmlM monad.

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

          -- we currently do not support other DTDs.
          render' (Doctype) = tell "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n"

          render' (Text text) = tell text

          -- for behaviour just generate some placeholder.
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

-- | The type representing time; in Haskell, it is just an empty declaration.

data Time

-- | 'Timed' is used for the definition of events. It is similar to 'Maybe',
-- but carries additional time information.

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

-- | timed-folding functions; it is the same as 'evfold'. (Or rather 'evfold'
-- is the same as this one.)

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

-- | Wraps a value in additional layer of 'Bhv'.

bhvWrap :: BhvFun a (Bhv a)
bhvWrap = primOp (prim . BhvConstEval) "bhv_wrap"

-- | Removes a layer of 'Bhv'.

bhvUnwrap :: BhvFun (Bhv a) a
bhvUnwrap = primOp (flip unsafeBfEval novalue) "bhv_unwrap"

-- | Turns behaviour function into a function between behaviours. Basically an
-- inverse of haskToBhv.

bhvToHask :: BhvFun a b -> (Bhv a -> Bhv b)
bhvToHask = (.)

-- | Turns function between behaviours into a behaviour function. Basically an
-- inverse of bhvToHask.

haskToBhv :: (Bhv a -> Bhv b) -> BhvFun a b
haskToBhv f = bhvUnwrap . apply . (cb f &&& bhvWrap)


-- | Type denoting raw JavaScript code

newtype RawJS = RawJS { unRawJS :: String }
    deriving Eq
instance IsString RawJS where fromString = RawJS

escapeStringJS = (>>=helper)
    where helper '\n' = "\\n"
          helper c | c `elem` "'\"\\" = '\\':c:[]
                   | otherwise        = [c]
