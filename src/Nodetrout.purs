-- | This module exports Nodetrout's public API.
module Nodetrout (serve, serve', module Error) where

import Prelude

import Data.MediaType (MediaType)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Node.HTTP (Request, Response) as NH
import Nodetrout.Internal.Content (class ResponseWritable)
import Nodetrout.Internal.Error
  ( HTTPError
  , error300
  , error301
  , error302
  , error303
  , error304
  , error305
  , error307
  , error400
  , error401
  , error402
  , error403
  , error404
  , error405
  , error406
  , error407
  , error409
  , error410
  , error411
  , error412
  , error413
  , error414
  , error415
  , error416
  , error417
  , error418
  , error422
  , error500
  , error501
  , error502
  , error503
  , error504
  , error505
  ) as Error
import Nodetrout.Internal.Router (class Router)
import Nodetrout.Internal.Server.Node as NS
import Type.Proxy (Proxy)

-- | Creates a `node-http`-compatible request handler of type
-- | `Request -> Response -> Effect Unit` which executes the specified routing
-- | logic.
serve
  :: forall layout handlers m content
   . Monad m
  => MonadEffect m
  => ResponseWritable content
  => Router layout (Record handlers) m (Tuple MediaType content)
  => Proxy layout
  -> Record handlers
  -> (m ~> Aff)
  -> (Error -> Effect Unit)
  -> NH.Request
  -> NH.Response
  -> Effect Unit
serve = NS.serve -- Note that the `serve` function is redeclared so that it appears at the top of the generated docs.

-- | A version of `serve` that can be used where handlers run in
-- | `ExceptT HTTPError Aff`, i.e. the inner monad is already `Aff`.
serve'
  :: forall layout handlers content
   . ResponseWritable content
  => Router layout (Record handlers) Aff (Tuple MediaType content)
  => Proxy layout
  -> Record handlers
  -> (Error -> Effect Unit)
  -> NH.Request
  -> NH.Response
  -> Effect Unit
serve' layout handlers = NS.serve layout handlers identity
