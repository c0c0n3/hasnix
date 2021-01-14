{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Data.Proxy (Proxy (Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

import API (ExprAPI, handlers)


exprAPI ∷ Proxy ExprAPI
exprAPI = Proxy

app ∷ Application
app = serve exprAPI handlers

{-
    $ cd components
    $ cabal run servo
-}
main ∷ IO ()
main = run 8080 app
