-- Rawe.hs: basic interface module
-- as part of rawe - ReActive Web Framework
--
-- Copyright 2011 Roman Smrž <roman.smrz@seznam.cz>, see file LICENSE


-----------------------------------------------------------------------------
-- |
-- Maintainer  : Roman Smrž <roman.smrz@seznam.cz>
-- Stability   : experimental
--
-- This module just re-exports some basic functions of the framework, monst
-- notably 'render' for turning a objects of type 'Html' into actual HTML code.
-- Suitable for modules needing just this functionality and not the combinators
-- provided by 'FRP.Rawe.Prelude' and 'FRP.Rawe.Html'.


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

    -- * Evaluation to Haskell functions
    unsafeBfEval, unsafeEval,
) where


import FRP.Rawe.Internal
