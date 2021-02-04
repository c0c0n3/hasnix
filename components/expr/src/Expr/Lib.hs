--
-- The library interface offered to users of this package.
-- Notice how some of the definitions exported by the internal module
-- stay private to this package---i.e. you won't be able to use them
-- in other packages. The Cabal file enforces that.
--
module Expr.Lib
  ( ExprF (..)
  , evalF
  , viewF
  )
where

import Expr.Internal
