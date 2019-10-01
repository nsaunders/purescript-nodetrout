module Example.ReqBody where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Effect (Effect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout.Error (HTTPError)
import Nodetrout.Server (serve)
import Text.Smolder.HTML (span)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), ReqBody, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML, HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

newtype Dimensions = Dimensions (Array Int)

undimensions :: Dimensions -> Array Int
undimensions (Dimensions d) = d

instance encodeHTMLPath :: EncodeHTML Dimensions where
  encodeHTML = span <<< text <<< String.joinWith " x " <<< map show <<< undimensions

type Site = "dimensions" := ReqBody (Array Int) JSON :> Resource (Post Dimensions HTML)

site :: Proxy Site
site = Proxy

resources :: forall m. Monad m => { dimensions :: Array Int -> { "POST" :: ExceptT HTTPError m Dimensions } }
resources = { dimensions: \d -> { "POST": pure $ Dimensions d } }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources identity
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
