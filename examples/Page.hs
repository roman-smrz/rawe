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



{- Example 5 - multiple pages with links -}

-- several sample pages, parts from previous examples

pages :: Bhv [(String, Html)]
pages = cb $
    [ ("text", div_ $ do
        b <- textfield "b"
        bhv $ toHtml $ length b
        )
    , ("server", div_ $ do
        let count = sget "count" :: Bhv (Maybe Int)
        bhv $ toHtml count
        )
    , ("event", div_ $ do
        a <- button ! value "+1"
        bhv $ toHtml $ timedFold (const (+)) (0::Bhv Int) $ fmap (const 1) a
        br
        )
    ]

page5 = html $ do
    head_ $ do
        title "Rawe - example 5"
    body $ do
        -- the links generates events for clicking ...
        p1 <- ae "text" $ "Textfields"
        br
        p2 <- ae "server" $ "Server get"
        br
        p3 <- ae "event" $ "Event fold"
        br

        --- which can be aggregated
        let pm = P.foldl1 (evmerge const) [p1, p2, p3] 

        -- and then used to choose a page
        bhv $ timed "" (\_ k -> maybe "" id (lookup k pages)) pm
