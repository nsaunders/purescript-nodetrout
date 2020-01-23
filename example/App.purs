module Example.App where

import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
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

newtype AppM a = AppM (ReaderT Message Aff a)

derive instance newtypeAppM :: Newtype (AppM a) _

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAskMessageAppM :: MonadAsk Message AppM
derive newtype instance monadReaderMessageAppM :: MonadReader Message AppM

runAppM :: forall a. Message -> AppM a -> Aff a
runAppM message = flip runReaderT message <<< un AppM

type Site = "message" := Resource (Get Message HTML)

site :: Proxy Site
site = Proxy

resources :: { message :: { "GET" :: ExceptT HTTPError AppM Message } }
resources = { message: { "GET": ask } }

shakedown :: forall r m a. Monad m => MonadReader Message m => r -> ExceptT HTTPError m a -> ExceptT HTTPError m a
shakedown _ x = local (const $ Message "Temp Message") x
--shakedown :: forall r m a. Monad m => r -> ExceptT HTTPError m a -> ExceptT HTTPError m a
--shakedown _ _ = throwError error402 { details = Just "You have to pay me first." }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources (runAppM $ Message "Hello, Sailor!") shakedown (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
