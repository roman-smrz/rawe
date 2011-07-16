{-# LANGUAGE OverloadedStrings #-}

module Page where

import qualified Prelude as P

import FRP.Rawe
import FRP.Rawe.Prelude
import FRP.Rawe.Html


{- Example 1 – using behaviours and textfields -}

page1 = html $ do
    head_ $ do
        title "Rawe - example 1"
    body $ do
        a <- textfield "a"
        bhv $ toHtml a
        br
        b <- textfield "b"
        bhv $ toHtml $ length b


{- Value loaded from server -}

page2 = html $ do
    head_ $ do
        title "Rawe - example 2"
    body $ do
        let count = sget "count" :: Bhv (Maybe Int)
        bhv $ toHtml count
