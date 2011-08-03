{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoRec #-}

-- Page.hs: example pages
-- as part of rawe - ReActive Web Framework
--
-- Copyright 2011 Roman Smrž <roman.smrz@seznam.cz>, see file LICENSE

-- This module contains several example pages assigned to variables page<n>,
-- which can be used to choose among them in the main module.


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
        -- texfields give behaviour of their value
        a <- textfield
        -- which can be displayed ...
        bhv $ toHtml a
        br
        b <- textfield
        -- ... or counted
        bhv $ toHtml $ length b


{- Value loaded from server -}

page2 = html $ do
    head_ $ do
        title "Rawe - example 2"
    body $ do
        -- value from server may simply displayed
        let count = sget "count" :: Bhv (Maybe Int)
        bhv $ toHtml count


{- Example 3 – event folds -}

page3 = html $ do
    head_ $ do
        title "Rawe - example 3"
    body $ do
        -- button generates events for clicking
        a <- button ! value "+1"
        -- we map give each of them a value 1 and then sum them
        bhv $ toHtml $ evfold (const (+)) (0::Bhv Int) $ fmap (const 1) a
        br

        -- changing behaviour can be also used as values for the event from
        -- a button:
        text <- textfield
        addtext <- button ! value "->"
        bhv $ toHtml $ timedFold (const (++)) "" $ fmap (const text) addtext


{- Example 4 - sending form data -}

page4 = html $ do
    head_ $ do
        title "Rawe - example 4"
    body $ do
        -- generate a form ...
        req <- form $ do textfield ! name "a"; br
                         str "+"; br
                         textfield ! name "b"; br
                         submit ! value "="
        -- and send data to a server (and display result)
        sum <- post "sum" req :: HtmlM (Bhv (Maybe Int))
        bhv $ toHtml sum



{- Example 5 - multiple pages with links -}

-- several sample pages, parts from previous examples

pages :: Bhv [(String, Html)]
pages = cb $
    [ ("text", div_ $ do
        b <- textfield
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


{- Example 6 - self-replacing form -}

page6 = html $ do
    head_ $ do
        title "Rawe - example 5"
    body $ do
            -- send the form only, if it passed the check
        rec result <- post "register" formData' :: HtmlM (Bhv (Maybe Int))
            (formData, formData') <- P.fmap (id &&& evguard checkForm) $ bhv $ (
                -- the form itself
                cb $ form $ do
                    textfield ! name "name"
                    br
                    textfield ! name "pass" ! type_ "password"
                    br
                    textfield ! name "pass-check" ! type_ "password"
                    br
                    bhv $ ("Password mismatch"::Bhv Html) `displayUnless`
                            timed true (const checkForm) formData
                    br
                    submit
                ) `until` (
                -- once sent replace with saying so
                fmap (const "Sending ...") $ timed nothing (\_ -> just) formData'
                ) `until` (
                -- after the anwer was received, display OK
                fmap (const "OK") result
                )

        P.return ()

-- just check if pass and pass-check agree
checkForm :: Bhv [(String, String)] -> Bhv Bool
checkForm x = lookup "pass" x == lookup "pass-check" x

-- display the html only if the condition does not hold
displayUnless :: (ToHtmlBhv a) => Bhv a -> Bhv Bool -> Bhv Html
displayUnless what = toHtml . bool nothing (just what)
