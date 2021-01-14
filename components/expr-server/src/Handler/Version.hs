{-# LANGUAGE UnicodeSyntax            #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
--
-- The @version@ resource.
-- It tells what's the sever version declared in the Cabal package.
-- Example:
-- @
--   $ curl localhost:8080/version
--   {"serverVersion":"0.1.0.0"}
-- @
module Handler.Version
  ( VersionEndpoint
  , versionHandler
  )
where

import Servant (Get, JSON, Server, (:>))

import Handler.Types.Version (ServerVersion, currentVersion)


type VersionEndpoint = "version" :> Get '[JSON] ServerVersion

versionHandler ∷ Server VersionEndpoint
versionHandler = pure currentVersion
