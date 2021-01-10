{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Test.Tasty

import qualified Expr.InternalTest as I


allTests = testGroup "Expr Tests"
  [ I.tests
  ]

main ∷ IO ()
main = defaultMain allTests
