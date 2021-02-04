{-# LANGUAGE UnicodeSyntax #-}
module ExprBuilder where

import Text.Printf (printf)

import Expr
import Util


-- Build an expression bin tree from a list of leaves by alternating
-- two bin ops to make inner nodes. E.g.
-- @
--    [1..5]  ~~>  (* (+ 1 2) (* (+ 3 4) 5))
--
--                           *
--                         /    \
--                        /      *
--                       /      / \
--                      +      +   \
--                     / \    / \   \
--                    1   2  3   4   5
-- @
-- Use 'printExpr' below to get sexprs, e.g. @printExpr [1..5]@.
--
expr ∷ (ξ → ξ → ξ) → (ξ → ξ → ξ) → ξ → [ξ] → ξ
expr plus times zero = build
  where
    build []         = zero
    build [x]        = x
    build [x, y]     = x `plus` y
    build (x:y:rest) = (x `plus` y) `times` (build rest)

plainExpr ∷ [Int] → Int
plainExpr = expr (+) (*) 0

taglessExpr ∷ [Int] → Int
taglessExpr = expr add mult 0 ∘ map lit

printExpr ∷ [Int] → Text
printExpr = expr add mult mempty ∘ map lit
