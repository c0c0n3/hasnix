{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- The library implementation. The stuff we export from this module
-- is only available to other code within the same package so we can
-- have an easier time testing the functionality.
--
module Expr.Internal
  ( Expr (..)
  , eval
  , ExprF (..)
  , evalF
  , viewF
  )
where

import Data.Text (pack)

import Util


-- Old school expression.
data Expr = Lit Int
          | Add Expr Expr
    deriving Show

-- Old school 'Expr' interpreter.
eval ∷ Expr → Int
eval (Lit x)   = x
eval (Add x y) = eval x + eval y


-- Tagless final 'Expr'.
-- Appending an /F/ to remind me I'd like to think of this as a clever
-- encoding of an (initial) F-algebra. (If my memory serves me well,
-- someone actually made that connection explicit.)
class ExprF repr where
  lit ∷ Int → repr
  add ∷ repr → repr → repr

-- Tagless final interpreter: calculator.
instance ExprF Int where
  lit = id
  add = (+)

evalF ∷ Int → Int
evalF = id

-- Tagless final interpreter: pretty-printer.
-- Print an S-expression: you can enter the string into a Lisp REPL to
-- make it print the evaluation result.
instance ExprF Text where
  lit       = pack ∘ show
  x `add` y = "(+ " <> x <> " " <> y <> ")"

viewF ∷ Text → Text
viewF = id
