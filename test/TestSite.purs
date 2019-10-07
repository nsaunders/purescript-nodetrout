module TestSite where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject)
import Data.Newtype (class Newtype)
import Nodetrout.Error (HTTPError)
import Nodetrout.Router (route)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout
  ( type (:/)
  , type (:<|>)
  , type (:=)
  , type (:>)
  , Capture
  , CaptureAll
  , QueryParam
  , QueryParams
  , ReqBody
  , Resource
  )
import Type.Trout.ContentType.HTML (HTML, class EncodeHTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get, Post)
import Type.Trout.PathPiece (class FromPathPiece, class ToPathPiece)

data Default = Default

instance encodeJsonDefault :: EncodeJson Default where
  encodeJson _ = jsonEmptyObject

instance encodeHTMLDefault :: EncodeHTML Default where
  encodeHTML _ = h1 $ text "Home Page"

newtype Message = Message { id :: Int, content :: String, unread :: Boolean }

derive instance newtypeMessage :: Newtype Message _

derive newtype instance decodeJsonMessage :: DecodeJson Message

derive newtype instance encodeJsonMessage :: EncodeJson Message

type Site = "default" := Resource (Get Default (JSON :<|> HTML))
       :<|> "messages" := "api" :/ "messages" :/ QueryParams "content" :/ QueryParam "unread" :/ Resource (Get (Array Message) JSON)
       :<|> "messageById" := "api" :/ "messages" :/ Capture "id" Int :/ Resource (Get Message JSON)
       :<|> "messagesById" := "api" :/ "messages" :/ CaptureAll "id" Int :/ Resource (Get (Array Message) JSON)
       :<|> "newMessage" := "api" :/ "messages" :/ ReqBody Message JSON :> Resource (Post Message JSON)

site :: Proxy Site
site = Proxy

type Handler m = ExceptT HTTPError m

messages :: Array Message
messages = Messages <$> [ { id: 1, content: "Hello", unread: true }
                        , { id: 2, content: "Hi", unread: true }
                        , { id: 3, content: "How are you today?", unread: false }
                        ]

messageHasContent :: String -> Message -> Boolean
messageHasContent c (Message { content }) = String.contains (String.toLower content) (Pattern $ String.toLower c)

messageUnread :: Message -> Boolean
messageUnread (Message { unread }) = unread

resources
  :: forall m
   . Monad m
  => { default :: { "GET" :: Handler m Default }
     , messages :: Array String -> Boolean -> { "GET" :: Handler m (Array Message) }
     , messageById :: Int -> { "GET" :: Handler m Message }
     , messagesById :: Array Int -> { "GET" :: Handler m (Array Message) }
     , newMessage :: Message -> { "POST" :: Handler m Message }
     }
resources =
  { default: { "GET": pure Default }
  , messages: \con unr -> { "GET": pure $ filter (\m -> messageHasContent con m || messageUnread m == unr) messages }
  , addMessage: \message -> { "POST": pure message }
  }
