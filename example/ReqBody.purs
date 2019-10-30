module Example.ReqBody where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, serve)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

newtype List = List (Array String)

reverseList :: List -> List
reverseList (List items) = List $ reverse items

instance decodeJsonList :: DecodeJson List where
  decodeJson = map List <<< decodeJson

instance encodeJsonList :: EncodeJson List where
  encodeJson (List items) = encodeJson items

type Site = "reversed" := ReqBody List JSON :> Resource (Post List JSON)

site :: Proxy Site
site = Proxy

resources :: forall m. Monad m => { reversed :: List -> { "POST" :: ExceptT HTTPError m List } }
resources = { reversed: \list -> { "POST": pure $ reverseList list } }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources identity (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
