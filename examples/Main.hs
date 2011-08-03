-- Main.hs: main example module
-- as part of rawe - ReActive Web Framework
--
-- Copyright 2011 Roman Smrž <roman.smrz@seznam.cz>, see file LICENSE

-- This module contains the main function used to get the examples working.
-- Here is constructed a simple server using Happstack, which server necessary
-- JavaScript files and a page chosen from the examples in Page.hs.

module Main where

import Control.Monad
import Control.Monad.Trans

import Happstack.Server

import FRP.Rawe

import Page


example = page1

main = do
        putStrLn "--- Server started ---"
        simpleHTTP (nullConf { port = 8080 }) $ msum [ mzero
            , dir "js" $ serveDirectory DisableBrowsing [] "js"

            , do methodM GET
                 name <- look "q"
                 ok $ toResponse $ case name of
                                        "count" -> "42" :: String

            , do methodM POST
                 decodeBody $ defaultBodyPolicy "/tmp/" 0 1000 1000
                 name <- look "q"
                 case name of
                      "sum" -> do a <- look "a"; b <- look "b"
                                  ok $ toResponse $ show $ (read a) + (read b :: Int)

            , return . setHTML . toResponse =<< liftIO (render example)
            ]


setHTML resp = resp { rsHeaders = setHeader "Content-Type" "text/html" $ rsHeaders resp }
