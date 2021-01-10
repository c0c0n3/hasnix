{-# LANGUAGE UnicodeSyntax #-}
module Expr.InternalTest (tests) where

import Hedgehog hiding (eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Expr.Internal
import Util


toExprF ∷ ExprF ξ ⇒ Expr → ξ
toExprF (Lit x)   = lit x
toExprF (Add x y) = toExprF x `add` toExprF y

genInt ∷ MonadGen m ⇒ m Int
genInt = Gen.integral (Range.linear 1 3)

genExpr ∷ MonadGen m ⇒ m Expr
genExpr =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Lit <$> genInt
    ] [
      -- recursive generators
      Gen.subterm2 genExpr genExpr Add
    ]

prop_eval ∷ Property
prop_eval = property $ do
  e ← forAll genExpr
  eval e === toExprF e

tests :: TestTree
tests = testGroup "Internal"
  [ testProperty "old school eval = tagless final eval" prop_eval
  ]
