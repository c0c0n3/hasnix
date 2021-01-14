{-# LANGUAGE UnicodeSyntax                            #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric            #-}
{-# LANGUAGE OverloadedStrings                        #-}
--
-- The @expr@ resource.
-- POST a JSON representation of an @expr@ to have the server evaluate
-- it for you and redirect you to a URL where you can GET the answer.
-- The answer too is a resource whose URL includes both the evaluation
-- result and the original @expr@ rendered as a @sexpr@.
-- Example (URLs not encoded and HTTP response simplified for clarity):
-- @
--   $ curl -v -X POST localhost:8080/expr \
--             -H "Content-Type: application/json" \
--             -d '{"add":[{"mult":[{"mult":[{"add":[1,5]},2]},4]},-6]}'
--
--   HTTP 303
--   Location: /expr/42/(+ (* (* (+ 1 5) 2) 4) -6)
--                   ^    ^
--         result ___|    |___ sexpr
--
--  $ curl localhost:8080/expr/42/(+ (* (* (+ 1 5) 2) 4) -6)
--    42
-- @
--
-- NOTE. I generated the JSON representation in the example above with
-- @
--   e = Add (Mult (Mult (Add (Lit 1) (Lit 5)) (Lit 2)) (Lit 4)) (Lit (-6))
--   encode e
-- @
module Handler.Expr
  ( ExprEndpoint
  , exprHandler
  )
where

import Data.Proxy (Proxy(Proxy))
import Servant (Capture, CaptureAll, Get, Handler, Link, JSON, ReqBody, Server
               , safeLink, (:<|>) ((:<|>)), (:>))

import Expr (evalF, viewF)
import Handler.Redirect (PostRedirect, RedirectResponse, redirect)
import Handler.Types.ExprTerm (ExprTerm(..), fromJsonExpr)
import Handler.Types.JsonExpr (JsonExpr)
import Util


type GetRoute = "expr" :>
  Capture "result" Int :> CaptureAll "sexpr" Text :> Get '[JSON] Int

type PostRoute = "expr" :>
  ReqBody '[JSON] JsonExpr :> PostRedirect 201 Link

type ExprEndpoint = GetRoute :<|> PostRoute

-- TODO output an absolute URI in the Location header.
toRedirectUrl ∷ ExprTerm → Link
toRedirectUrl (Term expr) = safeLink getRoute getRoute result [sexpr]
  where
    getRoute        = Proxy ∷ Proxy GetRoute
    (sexpr, result) = (viewF expr, evalF expr)
-- NOTE   ^ this works b/c we made `expr` polymorphic so the first
-- occurrence on the right is a Text whereas the second is an Int.

getHandler ∷ Int → [Text] → Handler Int
getHandler result _ = pure result

postHanlder ∷ JsonExpr → Handler (RedirectResponse Link)
postHanlder = redirect ∘ toRedirectUrl ∘ fromJsonExpr

exprHandler ∷ Server ExprEndpoint
exprHandler = getHandler :<|> postHanlder
