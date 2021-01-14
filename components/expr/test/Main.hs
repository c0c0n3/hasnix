{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Test.Tasty

import qualified Expr.InternalTest as I


allTests = testGroup "Expr Tests"
  [ I.tests
  ]

{-
    $ cd components
    $ cabal test expr      # or
    $ cabal run expr-test  # to see Tasty's output
-}
main ∷ IO ()
main = defaultMain allTests
