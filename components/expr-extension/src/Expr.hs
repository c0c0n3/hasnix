{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE OverloadedStrings #-}
module Expr
  ( module Expr.Lib
  , MultF (..)
  )
where

import Expr.Lib
import Util


-- Extend the expr language with new syntax.
class MultF repr where
  mult ∷ repr → repr → repr

-- Tagless final interpreter for the new syntax: calculator.
instance MultF Int where
  mult = (*)

-- Tagless final interpreter for the new syntax: pretty-printer.
-- Print an S-expression: you can enter the string into a Lisp REPL
-- to make it print the evaluation result. E.g.
-- @
--     ELISP> (+ (* (* (+ 1 5) 2) 4) -6)
--     42
-- @
instance MultF Text where
  x `mult` y = "(* " <> x <> " " <> y <> ")"
