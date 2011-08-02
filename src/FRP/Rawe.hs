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

    cb, bfix, haskToBhv,
    render,

    -- * To be (re)moved
    container, tag, jquery,
    span, reactive, reactive_prim, initReactive,
    b_curry, b_uncurry, b_toJSString, b_fromJSString,
    BhvServer(..),
) where


import Prelude hiding (head,div,(.),id,fst,snd,span,curry,uncurry)

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

instance BhvValue JSString where
        bhvValue str = return.RawJS $ "cthunk('"++escapeStringJS (fromJSString str)++"')"


type BehaviourFun = BhvFun
type Behaviour a = Bhv a





bhvPack :: BhvFun a (Bhv a)
bhvPack = primFunc "bhv_pack"

bhvUnpack :: BhvFun (Bhv a) a
bhvUnpack = primFunc "bhv_unpack"




haskToBhv :: (Bhv a -> Bhv b) -> BhvFun a b
haskToBhv f = bhvUnpack . apply . (cb f &&& bhvPack)






data BhvServer a b = BhvServer String JSString
instance BhvPrim (BhvServer a b) a b where
        bhvPrim (BhvServer func name) = do
                jname <- bhvValue name
                return (func, [jname])













jquery = script ! type_ "text/javascript" ! src "js/jquery.js" $ ""
reactive = script ! type_ "text/javascript" ! src "js/reactive.js" $ ""
reactive_prim = script ! type_ "text/javascript" ! src "js/reactive-prim.js" $ ""

initReactive :: Html
initReactive = do
        bs <- gets $ reverse . hsBehaviours
        hs <- gets $ reverse . hsHtmlValues
        hbs <- gets $ reverse . hsHtmlBehaviours
        hgs <- gets $ reverse . hsHtmlGens
        script ! type_ "text/javascript" $ do
                str $ "$(document).ready(function() {\n"

                forM_ bs $ \(i, func, params) ->
                        str $ "r_bhv_fun_0["++show i++"] = new BhvFun("++show i++");\n"

                forM_ hs $ \(i, h, mi) ->
                    str $ "var r_html_"++show i++" = $('"++escapeStringJS h++"')" ++
                    case mi of { Nothing -> ""; Just inner -> ".prop('rawe_html_inner', "++inner++")" }
                    ++ ";\n"

                forM_ hbs $ \(i, mv) ->
                    let var = case mv of Nothing -> "$('body')"
                                         Just v  -> "r_html_"++show v
                     in str $ "r_bhv_fun_0["++show i++"].html = "++var++".find('*[bhv-id="++show i++"]');\n"

                forM_ hgs $ \(i, mv) ->
                    let var = case mv of Nothing -> "$('body')"
                                         Just v  -> "r_html_"++show v
                     in str $ "r_bhv_fun_0["++show i++"].gen = "++var++".find2('*[bhv-gen="++show i++"]');\n"

                forM_ bs $ \(i, func, params) -> do
                        let jid = show i
                            jfunc = "r_prim_"++func
                            jparams = concatMap ((',':).unRawJS) params
                        str $ jfunc++".call(r_bhv_fun_0["++jid++"]"++jparams++");\n"

                str $ "r_init();\n"
                str $ "});\n";




container :: String -> Html -> Html
container tag (HtmlM f) = HtmlM $ \s -> let ((), (content, s')) = f s
                                         in ((), ([Tag tag [] content], s'))

tag :: String -> Html
tag name = HtmlM $ \s -> ((), ([Tag name [] []], s))

script = container "script"
span = container "span"

type_ = AttrVal "type"
src = AttrVal "src"





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






b_toJSString :: Behaviour String -> Behaviour JSString
b_toJSString = unOp "to_js_string"

b_fromJSString :: Behaviour JSString -> Behaviour String
b_fromJSString = unOp "from_js_string"


bcurry :: BehaviourFun (a, b) c -> BehaviourFun d a -> BehaviourFun d b -> BehaviourFun d c
bcurry f x y = f . (x &&& y)

bjoin :: Behaviour (BehaviourFun a b) -> BehaviourFun a b
bjoin = Prim . BhvModifier "bjoin"

b_fix :: forall a. (Behaviour a -> Behaviour a) -> Behaviour a
b_fix = primOp1 "fix"


class BhvFix a where
        bfix :: (a -> a) -> a

instance BhvFix (Behaviour a) where
        bfix = b_fix

instance (BhvFix b, BhvCurrying (Behaviour a -> b) ps (Behaviour d)) => BhvFix (Behaviour a -> b) where
        bfix f = b_curryAll $ (.) $ bjoin $ b_fix ((\x -> cb $ haskToBhv $ f' x) . (.) . bjoin)
                where f' = b_uncurryAll . \f1 -> f (b_curryAll f1)


instance Eq (BehaviourFun a b) where
        _ == _ = error "Eq instance for behaviours is required for Num, but is meaningless"
instance Show (BehaviourFun a b) where
        show _ = error "Show instance for behaviours is required for Num, but is meaningless"
instance Num (BehaviourFun a Int) where
        fromInteger = Prim . BhvConst . fromInteger
        (+) = binOp "plus"
        (*) = binOp "times"
        abs = unOp "abs"
        signum = unOp "signum"

instance Num (BehaviourFun a (Maybe Int)) where
        fromInteger = Prim . BhvConst . Just . fromInteger
        (+) = binOp "plus_mb"
        (*) = binOp "times_mb"
        abs = unOp "abs_mb"
        signum = unOp "signum_mb"




newBhv :: HtmlM (Behaviour a)
newBhv = undefined

(<--) :: Behaviour a -> HtmlM (Behaviour a) -> Html
bhv <-- html = undefined


b_debug :: Behaviour String -> Behaviour a -> Behaviour a
b_debug = binOp "debug"




instance IsString (Behaviour String) where
        fromString = cb
