--
-- Convenience module to re-export goodies we use across the board
-- within the project:
-- * 'Text' type
-- * Unicode versions of common Haskell operators to add a touch of
--   style to our code :-)
--
module Util
  ( Text
  , module Prelude.Unicode
  )
where

import Data.Text
import Prelude.Unicode
-- Have a look at the `src` dir of
--   https://github.com/roelvandijk/base-unicode-symbols/
-- to see what's in the box. `Prelude.Unicode` exports the most common
-- symbols you may want to use but there are also many others you could
-- import from the other modules in `src`.
