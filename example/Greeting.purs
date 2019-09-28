module Example.Greeting where

import Prelude
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing))
import Data.Array.NonEmpty (NonEmptyArray, cons', head, toArray)
import Effect (Effect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout.Error (HTTPError)
import Nodetrout.Server (serve)
import Text.Smolder.HTML (span)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:<|>), Lit, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

newtype Greeting = Greeting { id :: Int, message :: String }

derive instance genericGreeting :: Generic Greeting _

instance showGreeting :: Show Greeting where
  show = genericShow

instance encodeJsonGreeting :: EncodeJson Greeting where
  encodeJson = genericEncodeJson

instance encodeHTMLGreeting :: EncodeHTML Greeting where
  encodeHTML = show >>> text >>> span

type Site = "greetings" := Lit "greetings" :> Resource (Get (Array Greeting) JSON)
       :<|> "greeting" := Lit "greeting" :> Resource (Get Greeting (JSON :<|> HTML))

site :: Proxy Site
site = Proxy

greetings :: NonEmptyArray Greeting
greetings = map Greeting $ cons' { id: 1, message: "Hi" }
                               [ { id: 2, message: "Hello" }
                               , { id: 3, message: "Hey" }
                               ]

resources
  :: forall m
   . Monad m
  => { greeting :: { "GET" :: ExceptT HTTPError (ReaderT (NonEmptyArray Greeting) m) Greeting }
     , greetings :: { "GET" :: ExceptT HTTPError (ReaderT (NonEmptyArray Greeting) m) (Array Greeting) }
     }
resources =
  { greeting: { "GET": asks head }
  , greetings: { "GET": asks toArray }
  }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources (flip runReaderT greetings)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
