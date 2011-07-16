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


{- Example 3 – event folds -}

page3 = html $ do
    head_ $ do
        title "Rawe - example 3"
    body $ do
        a <- button ! value "+1"
        bhv $ toHtml $ timedFold (const (+)) (0::Bhv Int) $ fmap (const 1) a
        br

        text <- textfield "text"
        addtext <- button ! value "->"
        bhv $ toHtml $ timedFold (const (++)) "" $ fmap (const text) addtext


{- Example 4 - sending form data -}

page4 = html $ do
    head_ $ do
        title "Rawe - example 4"
    body $ do
        req <- form $ do textfield "a"; br
                         str "+"; br
                         textfield "b"; br
                         submit ! value "="
        sum <- post "sum" req :: HtmlM (Bhv (Maybe Int))
        bhv $ toHtml sum
