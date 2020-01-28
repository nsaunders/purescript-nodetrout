module Example.App where

import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, tell)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Foreign.Object (lookup) as FO
import Node.HTTP (createServer, listen, requestHeaders)
import Node.HTTP (Request) as NH
import Nodetrout (HTTPError, error402)
import Nodetrout.Internal.Server.Node (serve)
import Text.Smolder.HTML (span)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.Method (Get)

newtype Message = Message String

derive instance newtypeMessage :: Newtype Message _

instance encodeHTMLMessage :: EncodeHTML Message where
  encodeHTML = un Message >>> text >>> span

newtype AppM a = AppM (ReaderT Message (WriterT (Array String) Aff) a)

derive instance newtypeAppM :: Newtype (AppM a) _

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAskMessageAppM :: MonadAsk Message AppM
derive newtype instance monadReaderMessageAppM :: MonadReader Message AppM
derive newtype instance monadTellLogAppM :: MonadTell (Array String) AppM
derive newtype instance monadWriterLogAppM :: MonadWriter (Array String) AppM

runAppM :: AppM ~> Aff
runAppM (AppM appM) = do
  Tuple x logMessages <- runWriterT (runReaderT appM $ Message "Hello, Sailor!")
  liftEffect $ logShow logMessages
  pure x

type Site = "message" := Resource (Get Message HTML)

site :: Proxy Site
site = Proxy

resources :: { message :: { "GET" :: ExceptT HTTPError AppM Message } }
resources = { message: { "GET": ask } }

messageFromHeader
  :: forall m a
   . Monad m
  => MonadReader Message m
  => MonadTell (Array String) m
  => NH.Request
  -> ExceptT HTTPError m a
  -> ExceptT HTTPError m a
messageFromHeader req x =
  case FO.lookup "x-message" $ requestHeaders req of
    Nothing -> do
      tell ["Handling request as normal..."]
      foo <- x
      tell ["Finished handling request as normal."]
      pure foo
    Just message -> do
      tell ["Handling request with alternate message..."]
      foo <- local (const $ Message message) x
      tell ["Finished handling request with alternate message."]
      pure foo

main :: Effect Unit
main = do
  server <- createServer $ serve site resources runAppM messageFromHeader (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
