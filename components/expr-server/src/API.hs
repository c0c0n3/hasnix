{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
--
-- Assemble the server API type and corresponding handlers.
--
module API
  ( ExprAPI
  , handlers
  )
where

import Servant (Server, (:<|>) ((:<|>)))

import Handler.Expr
import Handler.Version


type ExprAPI = ExprEndpoint :<|> VersionEndpoint

handlers ∷ Server ExprAPI
handlers = exprHandler :<|> versionHandler
