{-# LANGUAGE UnicodeSyntax                      #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
module Main (main) where
{-
NOTE. -Wno-all-missed-specialisations
We suppress this warning within this file since GHC moans about

    Could not specialise imported function ‘Text.Printf.$wformatString’
      when specialising ‘Text.Printf.formatString’
    Probable fix: add INLINABLE pragma on ‘Text.Printf.$wformatString’

which isn't something we're inclined to fix.
-}

import Text.Printf (printf)

import Expr


expr ∷ (ExprF ξ, MultF ξ) ⇒ ξ
expr = (((lit 1 `add` lit 5) `mult` (lit 2)) `mult` lit 4) `add` lit (-6)

{-
    $ cd components
    $ cabal run expro
-}
main ∷ IO ()
main = do
  putStrLn $  "The Answer to the Ultimate Question of Life, "
           <> "the Universe, and Everything:"
  printf "%s = %d" (viewF expr) (evalF expr)
