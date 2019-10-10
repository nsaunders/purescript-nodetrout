module Nodetrout.Server.Node where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Lazy (defer)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify_, new, read) as Ref
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
import Nodetrout.Error (HTTPError, _details, _statusCode, _overview)
import Nodetrout.Request (Request(..))
import Nodetrout.Router (class Router, route)
import Type.Proxy (Proxy)

convertRequest :: NH.Request -> Request
convertRequest req = Request
  { method: requestMethod req
  , url: requestURL req
  , headers: requestHeaders req
  , readString: defer \_ ->
      makeAff \done -> do
        chunks <- Ref.new []
        Stream.onData (requestAsStream req) \chunk -> Ref.modify_ (cons chunk) chunks
        Stream.onEnd (requestAsStream req) $ Ref.read chunks >>=
          case _ of
            [] ->
              done $ Right Nothing
            chx ->
              Buffer.concat chx >>= Buffer.toString UTF8 >>= Just >>> Right >>> done
        pure nonCanceler
  }

serve
  :: forall layout handlers m content
   . Monad m
  => MonadEffect m
  => ResponseWritable content
  => Router layout (Record handlers) (ExceptT HTTPError m (Tuple MediaType content))
  => Proxy layout
  -> Record handlers
  -> (m ~> Aff)
  -> NH.Request
  -> NH.Response
  -> Effect Unit
serve layout handlers runM req res = launchAff_ $ runM do
  let rs = responseAsStream res
  result <- runExceptT $ route layout handlers (convertRequest req)
  liftEffect $ case result of
    Left error -> do
      setStatusCode res $ error ^. _statusCode
      setHeader res "content-type" "text/plain"
      let body = error ^. _overview <> fromMaybe "" ((\d -> ": " <> d) <$> error ^. _details)
      _ <- Stream.writeString rs UTF8 body $ pure unit
      Stream.end rs $ pure unit
    Right (Tuple (MediaType contentType) content) -> do
      setStatusCode res 200
      setHeader res "content-type" contentType
      writeResponse rs content
      Stream.end rs $ pure unit

class ResponseWritable content where
  writeResponse :: Stream.Writable () -> content -> Effect Unit

instance responseWritableString :: ResponseWritable String where
  writeResponse stream content = Stream.writeString stream UTF8 content (pure unit) *> pure unit
