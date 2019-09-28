module Example.Greeting where

import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray, cons', toArray)
import Effect (Effect)
import Effect.Console (log)
import Network.HTTP (status404)
import Node.HTTP (createServer, listen)
import Nodetrout.Error (HTTPError(..))
import Nodetrout.Server (serve)
import Text.Smolder.HTML (span)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:<|>), Capture, Lit, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

newtype Greeting = Greeting { id :: Int, message :: String }

greetingId :: Greeting -> Int
greetingId (Greeting { id }) = id

derive instance genericGreeting :: Generic Greeting _

instance showGreeting :: Show Greeting where
  show = genericShow

instance encodeJsonGreeting :: EncodeJson Greeting where
  encodeJson = genericEncodeJson

instance encodeHTMLGreeting :: EncodeHTML Greeting where
  encodeHTML = show >>> text >>> span

type Site = "greetings" := Lit "greetings" :> Resource (Get (Array Greeting) JSON)
       :<|> "greeting" := Lit "greeting" :> Capture "id" Int :> Resource (Get Greeting (JSON :<|> HTML))

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
  => { greeting :: Int -> { "GET" :: ExceptT HTTPError (ReaderT (NonEmptyArray Greeting) m) Greeting }
     , greetings :: { "GET" :: ExceptT HTTPError (ReaderT (NonEmptyArray Greeting) m) (Array Greeting) }
     }
resources =
  { greeting: \id ->
    { "GET": do
        greeting <- asks (find (\g -> greetingId g == id))
        case greeting of
          Nothing ->
            throwError $ HTTPError { status: status404, details: Just $ "No greeting matches id " <> show id <> "." }
          Just g ->
            pure g
    }
  , greetings: { "GET": asks toArray }
  }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources (flip runReaderT greetings)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
