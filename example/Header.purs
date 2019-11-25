module Example.Header where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..), fst)
import Data.String.Base64 (decode)
import Data.String.CodePoints (drop, take) as String
import Data.String.Common (split, toLower) as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, serve')
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/), type (:>), Header, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Header (class FromHeader)
import Type.Trout.Method (Get)

newtype BasicAuth = BasicAuth (Tuple String String)

derive instance newtypeBasicAuth :: Newtype BasicAuth _

instance fromHeaderBasicAuth :: FromHeader BasicAuth where
  fromHeader headerValue
    | String.toLower (String.take 6 headerValue) /= "basic " = Left "Only Basic authorization is supported."
    | otherwise = do
        payload <- lmap (\e -> "Failed to decode header: " <> message e) $ decode (String.drop 6 headerValue)
        case String.split (Pattern ":") payload of
          [ user, pass ] ->
            pure $ BasicAuth $ Tuple user pass
          _ ->
            Left "The Authorization header is invalid."

newtype Greeting = Greeting String

derive instance newtypeGreeting :: Newtype Greeting _

instance encodeJsonGreeting :: EncodeJson Greeting where
  encodeJson = encodeJson <<< un Greeting

type Site = "admin" := "admin" :/ Header "Authorization" BasicAuth :> Resource (Get Greeting JSON)

site :: Proxy Site
site = Proxy

resources :: forall m. Monad m => { admin :: BasicAuth -> { "GET" :: ExceptT HTTPError m Greeting } }
resources = { admin: \auth -> { "GET": pure $ Greeting $ "Hello, " <> (fst $ un BasicAuth auth) } }

main :: Effect Unit
main = do
  server <- createServer $ serve' site resources (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
