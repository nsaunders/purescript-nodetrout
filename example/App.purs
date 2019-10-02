module Example.App where

import Prelude
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout.Error (HTTPError)
import Nodetrout.Server (serve)
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

runAppM :: forall a. Message -> AppM a -> Aff a
runAppM message = flip runReaderT message <<< un AppM

type Site = "message" := Resource (Get Message HTML)

site :: Proxy Site
site = Proxy

resources :: { message :: { "GET" :: ExceptT HTTPError AppM Message } }
resources = { message: { "GET": ask } }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources (runAppM $ Message "Hello, Sailor!")
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
