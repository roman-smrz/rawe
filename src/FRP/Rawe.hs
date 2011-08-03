{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs,
  FunctionalDependencies, OverloadedStrings,
  TypeSynonymInstances, NoMonomorphismRestriction #-}

{-# LANGUAGE DoRec #-}
{-# LANGUAGE UndecidableInstances #-}


module FRP.Rawe (
    -- * Core types
    Bhv, BhvFun,

    Time, Timed(..),
    timed, notYet, onTime,
    timedFold,

    -- * Utility functions

    bhvWrap, bhvUnwrap, haskToBhv, bhvToHask,
    cb, bfix,
    render,

    -- * To be (re)moved
    b_curry, b_uncurry,
    BhvServer(..),
) where


import Prelude hiding (head,div,(.),id,fst,snd,curry,uncurry)

import Control.Categorical.Bifunctor
import Control.Category
import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Category.Monoidal

import Control.Monad.State

import Data.String
import Data.Void

import Text.JSON

import FRP.Rawe.Internal

import Debug.Trace



{-
instance (BhvValue a) => BhvValue (Timed a) where
        bhvValue _ = error "Time is a phantom type"
        bhvValue (Timed t x) = do (RawJS jt) <- bhvValue t
                                  (RawJS jx) <- bhvValue x
                                  return.RawJS $ "{Timed:["++jt++","++jx++"]}"
        bhvValue NotYet = return.RawJS $ "{NotYet:null}"
        -}


type BehaviourFun = BhvFun
type Behaviour a = Bhv a









data BhvServer a b = BhvServer String JSString
instance BhvPrim (BhvServer a b) a (Maybe b) where
        bhvPrim (BhvServer func name) = do
                jname <- bhvValue name
                return (func, [jname])
        unsafeBhvEval _ = const Nothing






b_uncurry :: (Behaviour a -> Behaviour b -> c) -> Behaviour (a, b) -> c
b_uncurry f x = f (fst . x) (snd . x)

b_curry :: (Behaviour (a, b) -> c) -> Behaviour a -> Behaviour b -> c
b_curry f x y = f (x &&& y)

class BhvCurrying a ps r | a -> ps r where
        b_uncurryAll :: a -> Behaviour ps -> r
        b_curryAll :: (Behaviour ps -> r) -> a

instance BhvCurrying (Behaviour a -> Behaviour b) a (Behaviour b) where
        b_uncurryAll = id
        b_curryAll = id

instance (BhvCurrying (Behaviour b -> r) ps r') =>
        BhvCurrying (Behaviour a -> Behaviour b -> r) (a, ps) r' where

        b_uncurryAll f = b_uncurry (\x -> b_uncurryAll (f x))
        b_curryAll f = \x -> b_curryAll $ (b_curry f) x



{- Basic classes and functions -}

{-
b_liftM2 :: (BMonad m) => (Behaviour a -> Behaviour b -> Behaviour c) ->
        Behaviour (m a) -> Behaviour (m b) -> Behaviour (m c)
b_liftM2 f mx my = mx ~>>= \x -> my ~>>= \y -> b_return (f x y)
-}


bcurry :: BehaviourFun (a, b) c -> BehaviourFun d a -> BehaviourFun d b -> BehaviourFun d c
bcurry f x y = f . (x &&& y)

bjoin :: Behaviour (BehaviourFun a b) -> BehaviourFun a b
bjoin = prim . BhvModifier (unsafeBfEval . ($void)) "bjoin"

b_fix :: forall a. (Behaviour a -> Behaviour a) -> Behaviour a
b_fix = primOp1 (\f -> let x = f x in x) "fix"


class BhvFix a where
        bfix :: (a -> a) -> a

instance BhvFix (Behaviour a) where
        bfix = b_fix

instance (BhvFix b, BhvCurrying (Behaviour a -> b) ps (Behaviour d)) => BhvFix (Behaviour a -> b) where
        bfix f = b_curryAll $ (.) $ bjoin $ b_fix ((\x -> cb $ haskToBhv $ f' x) . (.) . bjoin)
                where f' = b_uncurryAll . \f1 -> f (b_curryAll f1)



newBhv :: HtmlM (Behaviour a)
newBhv = undefined

(<--) :: Behaviour a -> HtmlM (Behaviour a) -> Html
bhv <-- html = undefined


b_debug :: Behaviour String -> Behaviour a -> Behaviour a
b_debug = primOp2 (const id) "debug"
