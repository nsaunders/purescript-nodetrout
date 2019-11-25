module Example.ReqBody where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, un)
import Effect (Effect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, serve')
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

newtype ExampleList = ExampleList (Array String)

derive instance newtypeExampleList :: Newtype ExampleList _

instance decodeJsonList :: DecodeJson ExampleList where
  decodeJson = map ExampleList <<< decodeJson

instance encodeJsonList :: EncodeJson ExampleList where
  encodeJson = encodeJson <<< un ExampleList

type Site = "reversed" := ReqBody ExampleList JSON :> Resource (Post ExampleList JSON)

site :: Proxy Site
site = Proxy

resources :: forall m. Monad m => { reversed :: ExampleList -> { "POST" :: ExceptT HTTPError m ExampleList } }
resources = { reversed: \list -> { "POST": pure $ over ExampleList reverse list } }

main :: Effect Unit
main = do
  server <- createServer $ serve' site resources (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
