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

            , return $ setHTML $ toResponse $ render example
            ]


setHTML resp = resp { rsHeaders = setHeader "Content-Type" "text/html" $ rsHeaders resp }
