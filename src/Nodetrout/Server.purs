module Nodetrout.Server where
  
import Prelude
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (Request, Response, responseAsStream, setHeader, setStatusCode)
import Node.Stream (Writable, end, writeString) as Stream
import Nodetrout.Context (fromRequest) as Context
import Nodetrout.Router (class Router, route)
import Type.Proxy (Proxy)

serve
  :: forall layout handlers m content
   . Monad m
  => MonadEffect m
  => ResponseWritable content
  => Router layout (Record handlers) m (Tuple MediaType content)
  => Proxy layout
  -> Record handlers
  -> (m ~> Aff)
  -> Request
  -> Response
  -> Effect Unit
serve layout handlers runM req res = launchAff_ $ runM do
  let rs = responseAsStream res
  result <- runExceptT $ route layout handlers (Context.fromRequest req)
  liftEffect $ case result of
    Left error -> do
      setStatusCode res 501
      Stream.writeString rs UTF8 "Not implemented (fail)" (pure unit) *> Stream.end rs (pure unit)
    Right (Tuple (MediaType contentType) content) -> do
      setStatusCode res 200
      setHeader res "content-type" contentType
      writeResponse rs content
      Stream.end rs (pure unit)
      --Stream.writeString rs UTF8 rendered (pure unit) *> Stream.end rs (pure unit)

class ResponseWritable content where
  writeResponse :: Stream.Writable () -> content -> Effect Unit

instance responseWritableString :: ResponseWritable String where
  writeResponse stream content = Stream.writeString stream UTF8 content (pure unit) *> pure unit
