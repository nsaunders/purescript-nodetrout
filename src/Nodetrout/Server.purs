module Nodetrout.Server where
  
import Prelude
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (Request, Response, responseAsStream, setStatusCode)
import Node.Stream (end, writeString) as Stream
import Nodetrout.Context (fromRequest) as Context
import Nodetrout.Router (class Router, route)
import Type.Proxy (Proxy)

serve
  :: forall layout handlers m a
   . Monad m
  => MonadEffect m
  => Router layout (Record handlers) m a
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
    Right something -> do
      setStatusCode res 501
      Stream.writeString rs UTF8 "Not implemented (success)" (pure unit) *> Stream.end rs (pure unit)
      pure unit
