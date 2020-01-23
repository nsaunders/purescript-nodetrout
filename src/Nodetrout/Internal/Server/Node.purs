-- | This module contains the request handling logic related to `node-http`.
module Nodetrout.Internal.Server.Node (class ResponseWritable, serve, writeResponse) where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, runAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Ref (modify_, new, read, write) as Ref
import Node.Buffer (concat, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (Request, Response) as NH
import Node.HTTP
  ( requestHeaders
  , requestMethod
  , requestAsStream
  , requestURL
  , responseAsStream
  , setHeader
  , setStatusCode
  )
import Node.Stream (Writable, end, onData, onEnd, writeString) as Stream
import Nodetrout.Internal.Error (HTTPError)
import Nodetrout.Internal.Request (Request(..))
import Nodetrout.Internal.Router (class Router, route)
import Type.Proxy (Proxy)

-- | Specifies the method, URL, headers, and body of a request. This abstraction
-- | exists so that the router can be tested with a fake request.
convertRequest :: NH.Request -> Effect Request
convertRequest req = do
  requestData <- Ref.new Nothing
  let body = makeAff \done -> do
        previousData <- Ref.read requestData
        case previousData of
          Just buffer ->
            done $ Right buffer
          Nothing -> do
            chunks <- Ref.new []
            Stream.onData (requestAsStream req) \chunk -> Ref.modify_ (cons chunk) chunks
            Stream.onEnd (requestAsStream req) $ Ref.read chunks >>= \chx -> do
              buffer <- Buffer.concat chx
              Ref.write (Just buffer) requestData
              done $ Right buffer
        pure nonCanceler
  pure $ Request
    { method: requestMethod req
    , url: requestURL req
    , headers: requestHeaders req
    , stringBody: find (_ /= "") <<< Just <$> (liftEffect <<< Buffer.toString UTF8 =<< body)
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
  -> (NH.Request -> ExceptT HTTPError m (Tuple MediaType content) -> ExceptT HTTPError m (Tuple MediaType content))
  -> (Error -> Effect Unit)
  -> NH.Request
  -> NH.Response
  -> Effect Unit
serve layout handlers runM foo onError req res =
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
      result <- runExceptT $ foo req (route layout handlers request 0)
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

-- | Specifies how to write `content` to a response stream.
class ResponseWritable content where
  writeResponse :: Stream.Writable () -> content -> Effect Unit

instance responseWritableString :: ResponseWritable String where
  writeResponse stream content = Stream.writeString stream UTF8 content (pure unit) *> pure unit
