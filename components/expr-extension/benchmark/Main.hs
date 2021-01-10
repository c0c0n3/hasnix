{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Criterion.Main

import ExprBuilder (plainExpr, taglessExpr)
import Util


type ExprBuilder = [Int] → Int
type InputSize   = Int
type Description = String

benchFor ∷ ExprBuilder → InputSize → Benchmark
benchFor builder size = bench desc $ nf builder [1..size]
  where
    desc = "input size: " <> show size

group ∷ Description → (InputSize → Benchmark) → Benchmark
group desc benchBuilder = bgroup desc $ map benchBuilder sizes
  where
    sizes = take 4 ∘ iterate (*10) $ 100

-- NOTE. Criterion report.
-- To see a summary report on the CLI, just ask Cabal to run the benchmark
-- @
--     $ cabal bench expr-extension
-- @
-- For a detailed report
-- @
--     $ cabal run expr-bench -- --output benchmark.html
-- @
main ∷ IO ()
main = defaultMain
  [ group "Plain Expressions" $ benchFor plainExpr
  , group "Tagless Expressions" $ benchFor taglessExpr
  ]
