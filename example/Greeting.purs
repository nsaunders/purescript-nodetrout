module Example.Greeting where

import Prelude
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array (replicate)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout.Error (HTTPError)
import Nodetrout.Server (serve)
import Text.Smolder.HTML (p)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:<|>), Lit, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

data Greeting = Greeting String

type Site = "greeting" := Lit "greeting" :> Resource (Get Greeting (JSON :<|> HTML))
       :<|> "greetings" := Lit "greetings" :> Resource (Get (Array Greeting) (JSON))

instance encodeJsonGreeting :: EncodeJson Greeting where
  encodeJson (Greeting g) = encodeJson g

instance encodeHTMLGreeting :: EncodeHTML Greeting where
  encodeHTML (Greeting g) = p (text g)

runAppM :: forall a. String -> ReaderT String Aff a -> Aff a
runAppM = flip runReaderT

site :: Proxy Site
site = Proxy

resources
  :: forall m
   . Monad m
  => { greeting :: { "GET" :: ExceptT HTTPError (ReaderT String m) Greeting }
     , greetings :: { "GET" :: ExceptT HTTPError (ReaderT String m) (Array Greeting) }
     }
resources =
  { greeting: { "GET": Greeting <$> ask }
  , greetings: { "GET": replicate 5 <<< Greeting <$> ask }
  }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources (runAppM "Hello")
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
