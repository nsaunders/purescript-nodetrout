-- | This module contains the request handling logic related to `node-http`.
module Nodetrout.Internal.Server.Node (serve) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Array (cons)
import Data.ByteString as ByteString
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, runAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Ref (modify_, new, read, write) as Ref
import Node.Buffer (concat) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (Request, Response) as NH
import Node.HTTP (requestHeaders, requestMethod, requestAsStream, requestURL, responseAsStream, setHeader, setStatusCode)
import Node.Stream (end, onData, onEnd, writeString) as Stream
import Nodetrout.Internal.Content (class ResponseWritable, writeResponse)
import Nodetrout.Internal.Request (Request(..))
import Nodetrout.Internal.Router (class Router, route)
import Type.Proxy (Proxy)

-- | Specifies the method, URL, headers, and body of a request. This abstraction
-- | exists so that the router can be tested with a fake request.
convertRequest :: NH.Request -> Effect Request
convertRequest req = do
  requestData <- Ref.new Nothing
  let 
      bytestringBody = makeAff \done -> do
        let finish buffer =
              -- | The unsafeFreeze here is OK, as we're only ever going to call it
              -- | when we know we got all the data from the request and the Buffer
              -- | won't be modified anymore
              let bs      = ByteString.unsafeFreeze buffer
                  maybeBS = if ByteString.isEmpty bs
                            then Nothing
                            else Just bs
               in done (Right maybeBS)
        previousData <- Ref.read requestData
        case previousData of
          Just buffer -> finish buffer
          Nothing -> do
            chunks <- Ref.new []
            Stream.onData (requestAsStream req) \chunk -> Ref.modify_ (cons chunk) chunks
            Stream.onEnd (requestAsStream req) $ Ref.read chunks >>= \chx -> do
              buffer <- Buffer.concat chx
              Ref.write (Just buffer) requestData
              finish buffer
        pure nonCanceler
  pure $ Request
    { method: requestMethod req
    , url: requestURL req
    , headers: requestHeaders req
    , bytestringBody
    }

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
serve layout handlers runM onError req res =
  let
    rs = responseAsStream res
    requestCallback = case _ of
      Right _ ->
        pure unit
      Left error -> do
        setStatusCode res 500
        setHeader res "content-type" "text/plain"
        _ <- Stream.writeString rs UTF8 "An unexpected error occurred while processing the request." $ pure unit
        Stream.end rs $ pure unit
        onError error
  in
    runAff_ requestCallback $ runM do
      request <- liftEffect $ convertRequest req
      result <- runExceptT $ route layout handlers request 0
      liftEffect $ case result of
        Left { statusCode, overview, details } -> do
          setStatusCode res statusCode
          setHeader res "content-type" "text/plain"
          let body = overview <> fromMaybe "" ((\d -> ": " <> d) <$> details)
          _ <- Stream.writeString rs UTF8 body $ pure unit
          Stream.end rs $ pure unit
        Right (Tuple (MediaType contentType) content) -> do
          setStatusCode res 200
          setHeader res "content-type" contentType
          writeResponse rs content
          Stream.end rs $ pure unit
