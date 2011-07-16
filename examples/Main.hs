module Main where

import Control.Monad

import Happstack.Server

import FRP.Rawe

import Page


example = page1

main = do
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

            , return $ setHTML $ toResponse $ render example
            ]


setHTML resp = resp { rsHeaders = setHeader "Content-Type" "text/html" $ rsHeaders resp }
