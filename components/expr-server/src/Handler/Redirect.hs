{-# LANGUAGE UnicodeSyntax             #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
--
-- It looks like Servant doesn't have built-in redirects?
-- I've taken the code below from this gist:
-- * https://gist.github.com/alpmestan/757094ecf9401f85c5ba367ca20b8900
-- See also:
-- * https://stackoverflow.com/questions/46789241/redirections-in-servant
-- * https://github.com/haskell-servant/servant/issues/117
--
module Handler.Redirect
  ( PostRedirect
  , RedirectResponse
  , redirect
  )
where

import GHC.TypeLits (Nat)
import Servant (Handler, Header, Headers, JSON, NoContent(NoContent)
               , StdMethod(POST), ToHttpApiData, Verb, addHeader)


type RedirectResponse loc = Headers '[Header "Location" loc] NoContent

type PostRedirect (statusCode ∷ Nat) loc
  = Verb 'POST statusCode '[JSON] (RedirectResponse loc)

redirect ∷ ToHttpApiData loc ⇒ loc → Handler (RedirectResponse loc)
redirect loc = pure $ addHeader loc NoContent
