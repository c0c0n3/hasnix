{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
--
-- Solve the de-serialisation problem.
--
-- After converting a JSON doc to 'JsonExpr', we still have to produce
-- an @expr@ term that any of the existing interpreters can process.
-- Plus, we want to be able to feed the same term to multiple interpreters
-- within the same lexical scope.
--
-- The easy way of doing it is to use polymorphic @expr@ values which
-- is what we do down below. But this solution isn't extensible, well
-- at least not in the spirit of the expression problem, since every time
-- we add a new interpreter we also have to add a new type class constraint
-- to this file---see below. This isn't really a train smash in our case,
-- also considering we didn't make 'JsonExpr' extensible either and that
-- would need tweaking too when adding a new interpreter.  Anyhoo, to
-- the brave soul seeking a mind-boggling solution, I say read:
-- *  http://okmij.org/ftp/tagless-final/course/lecture.pdf
--
module Handler.Types.ExprTerm
  ( ExprTerm(..)
  , fromJsonExpr
  )
where

import Expr (ExprF(..), MultF(..))
import Handler.Types.JsonExpr (JsonExpr(..))


type Interpreter ξ = (ExprF ξ, MultF ξ)
newtype ExprTerm = Term (∀ ξ . Interpreter ξ ⇒ ξ)


fromJsonExpr ∷ JsonExpr → ExprTerm
fromJsonExpr x = Term $ fromJsonExpr' x

fromJsonExpr' ∷ Interpreter ξ ⇒ JsonExpr → ξ
fromJsonExpr' (Lit n)    = lit  n
fromJsonExpr' (Add x y)  = add  (fromJsonExpr' x) (fromJsonExpr' y)
fromJsonExpr' (Mult x y) = mult (fromJsonExpr' x) (fromJsonExpr' y)

{- NOTE. New type wrapper.
Technically we don't need it, but w/o it, client modules would have
to turn on rankN types. In fact, we could've written fromJsonExpr
like this

from ∷ JsonExpr → (∀ ξ . Interpreter ξ ⇒ ξ)
from (Lit n)    = lit  n
from (Add x y)  = add  (from x) (from y)
from (Mult x y) = mult (from x) (from y)

then imported it in another module where we could've had code to
run different interpreters on the same (polymorphic) value

interpret2 ∷ (∀ ξ . Interpreter ξ ⇒ ξ) → (Text, Int)
interpret2 expr = (viewF expr, evalF expr)

e = Add (Mult (Mult (Add (Lit 1) (Lit 5)) (Lit 2)) (Lit 4)) (Lit (-6))
interpret2 (from e) ~~~> ("(+ (* (* (+ 1 5) 2) 4) -6)", 42)

but notice interpret2's signature: you'll need rankN types!
-}
