module Example.API where

import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array.NonEmpty (NonEmptyArray, cons', filter, toArray)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (contains) as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Network.HTTP (status404)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError(..), serve)
import Text.Smolder.HTML (span)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/), type (:>), type (:<|>), Capture, Lit, Resource, QueryParam)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

newtype Greeting = Greeting { id :: Int, message :: String }

greetingId :: Greeting -> Int
greetingId (Greeting { id }) = id

greetingMessage :: Greeting -> String
greetingMessage (Greeting { message }) = message

instance showGreeting :: Show Greeting where
  show (Greeting g) = show g

instance encodeJsonGreeting :: EncodeJson Greeting where
  encodeJson (Greeting g) = encodeJson g

instance encodeHTMLGreeting :: EncodeHTML Greeting where
  encodeHTML = show >>> text >>> span

type Site = "greetings" := Lit "greetings" :> QueryParam "filter" String :> Resource (Get (Array Greeting) JSON)
       :<|> "greeting" := "greetings" :/ Capture "id" Int :> Resource (Get Greeting (JSON :<|> HTML))

site :: Proxy Site
site = Proxy

type Handler m = ExceptT HTTPError (ReaderT (NonEmptyArray Greeting) m)

resources
  :: forall m
   . Monad m
  => { greeting :: Int -> { "GET" :: Handler m Greeting }
     , greetings :: Maybe String -> { "GET" :: Handler m (Array Greeting) }
     }
resources =
  { greeting: \id ->
    { "GET": do
        greetingMatch <- asks $ find ((_ == id) <<< greetingId)
        case greetingMatch of
          Just greeting ->
            pure greeting
          Nothing ->
            throwError $ HTTPError { status: status404, details: Just $ "No greeting matches id " <> show id <> "." }
    }
  , greetings: \messageFilter ->
      { "GET": asks $ case messageFilter of
          Just message -> filter ((String.contains $ Pattern message) <<< greetingMessage)
          Nothing -> toArray
      }
  }

main :: Effect Unit
main = do
  let greetings = map Greeting $ cons' { id: 1, message: "Hi" }
                                   [ { id: 2, message: "Hello" }
                                   , { id: 3, message: "Hey" }
                                   ]
  server <- createServer $ serve site resources (flip runReaderT greetings)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
