{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

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


import Control.Monad.Fix
import Control.Monad.State

import Data.String


-- * Basic definitions for HTML

-- | The type representing atrributes of HTML elements
data Attribute = AttrVal String String  -- ^ Key-value pair
               | AttrBool String        -- ^ Boolean attribute

-- | Type class used to enable adding attributes to values of both types
-- Html and Html -> Html.
class Attributable b where
        (!) :: b -> Attribute -> b



-- | The structure of HTML tree.
data HtmlStructure = Tag String [Attribute] [HtmlStructure] -- ^ Tag with content
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


instance IsString Html where
        fromString text = HtmlM $ \s -> ((), ([Text text], s))

-- | Function used to include text node into the HTML page. Forces the type
-- Html insted of arbitrary HtmlM a inferred when using overloaded strings
-- directly.

str :: String -> Html
str = fromString


-- * Behaviours



-- * Other definitions

newtype RawJS = RawJS { unRawJS :: String }
instance IsString RawJS where fromString = RawJS
