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
    cb,
    render,
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




{- Basic classes and functions -}

{-
b_liftM2 :: (BMonad m) => (Behaviour a -> Behaviour b -> Behaviour c) ->
        Behaviour (m a) -> Behaviour (m b) -> Behaviour (m c)
b_liftM2 f mx my = mx ~>>= \x -> my ~>>= \y -> b_return (f x y)
-}



newBhv :: HtmlM (Behaviour a)
newBhv = undefined

(<--) :: Behaviour a -> HtmlM (Behaviour a) -> Html
bhv <-- html = undefined


b_debug :: Behaviour String -> Behaviour a -> Behaviour a
b_debug = primOp2 (const id) "debug"
