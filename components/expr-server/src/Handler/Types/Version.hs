{-# LANGUAGE UnicodeSyntax                 #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
--
-- Pack our Cabal package version into a type that can be converted
-- to JSON.
-- We use the Cabal auto-generated 'Paths_expr_server' module to grab
-- the actual package version declared in the Cabal file.
--
module Handler.Types.Version
  ( ServerVersion
  , currentVersion
  )
where

import Data.Aeson (ToJSON)
import Data.Version (showVersion)
import GHC.Generics (Generic)

import Paths_expr_server (version)
-- NOTE. ^ hyphens in the package name get replaced by underscores.


data ServerVersion = ServerVersion { serverVersion ∷ String }
  deriving (Eq, Show, Generic, ToJSON)

currentVersion ∷ ServerVersion
currentVersion = ServerVersion $ showVersion version
