module Example.Header where

import Prelude
import Control.Monad.Except (ExceptT, except, throwError)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.String.Base64 (decode)
import Data.String.CodePoints (drop, take) as String
import Data.String.Common (split, toLower) as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, error400, serve)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), Header, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type Credentials = { username :: String, password :: String }

type Site = "credentials" := Header "Authorization" String :> Resource (Get Credentials JSON)

site :: Proxy Site
site = Proxy

resources :: forall m. Monad m => { credentials :: String -> { "GET" :: ExceptT HTTPError m Credentials } }
resources =
  { credentials:
      \auth ->
        { "GET": do
            when ((String.toLower $ String.take 6 auth) /= "basic ")
              $ throwError error400 { details = Just "Only Basic authorization is supported." }
            authStr <- except
                       $ lmap (\e -> error400 { details = Just ("Failed to decode header: " <> message e) })
                       $ decode $ String.drop 6 auth
            case String.split (Pattern ":") authStr of
              [ username, password ] ->
                pure { username, password }
              _ ->
                throwError error400 { details = Just "The Authorization header is invalid." }
        }
    }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources identity (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
