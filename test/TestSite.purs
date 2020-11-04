module Test.Site where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, jsonEmptyObject)
import Data.Array (filter)
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8) as ByteString
import Data.Either (Either(Left))
import Data.Foldable (find, foldr)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (class Newtype, un)
import Data.String (contains, toLower) as String
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Nodetrout (HTTPError, error404)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, CaptureAll, Header, QueryParam, QueryParams, ReqBody, Resource)
import Type.Trout.ContentType (class HasMediaType, class MimeParse, class MimeRender)
import Type.Trout.ContentType.HTML (HTML, class EncodeHTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get, Post)
import Type.Trout.PathPiece (class FromPathPiece)

data Default = Default

instance encodeJsonDefault :: EncodeJson Default where
  encodeJson _ = jsonEmptyObject

instance encodeHTMLDefault :: EncodeHTML Default where
  encodeHTML _ = h1 $ text "Home Page"

data MagicContentType

magicMimeType :: MediaType
magicMimeType = MediaType "application/magic"

magicMimeTypeRenderString :: String
magicMimeTypeRenderString = "It's magic, you know!"

instance hasMediaTypeMagic :: HasMediaType MagicContentType where
  getMediaType _ = magicMimeType

instance magicallyRenderMagicContentType :: MimeRender a MagicContentType ByteString where
  mimeRender _ _ = ByteString.toUTF8 magicMimeTypeRenderString

newtype Message = Message { id :: Int, content :: String, unread :: Boolean }

derive instance newtypeMessage :: Newtype Message _

derive newtype instance decodeJsonMessage :: DecodeJson Message

derive newtype instance encodeJsonMessage :: EncodeJson Message

newtype PathBoolean = PathBoolean Boolean

instance fromPathPiecePathBoolean :: FromPathPiece PathBoolean where
  fromPathPiece = map PathBoolean <<< case _ of
    "false" -> pure false
    "0" -> pure false
    "true" -> pure true
    "1" -> pure true
    unrecognized -> Left unrecognized

derive instance newtypePathBoolean :: Newtype PathBoolean _

data Admin = Admin String

instance encodeHTMLAdmin :: EncodeHTML Admin where
  encodeHTML (Admin username) = h1 $ text username

data ParseMethodDetect = ParsedJson | ParsedMagic

instance encodeJsonParseMethodDetect :: EncodeJson ParseMethodDetect where
  encodeJson ParsedJson = encodeJson { parsed: "json" }
  encodeJson ParsedMagic = encodeJson { parsed: "magic" }

instance decodeJsonParseMethodDetect :: DecodeJson ParseMethodDetect where
  decodeJson _ = pure ParsedJson

instance mimeParseParseMethodDetectMagic :: MimeParse ByteString MagicContentType ParseMethodDetect where
  mimeParse _ _ = pure ParsedMagic

type Site = "default" := Resource (Get Default (JSON :<|> HTML :<|> MagicContentType))
       :<|> "admin" := "admin" :/ Header "Authorization" String :> Resource (Get Admin HTML)
       :<|> "api" := "api" :/ (
         "messages" := "messages" :/ (
                "messages" := QueryParams "content" String :> QueryParam "unread" PathBoolean :> Resource (Get (Array Message) JSON)
           :<|> "messageById" := Capture "id" Int :> Resource (Get Message JSON)
           :<|> "messagesById" := CaptureAll "id" Int :> Resource (Get (Array Message) JSON)
           :<|> "newMessage" := ReqBody Message JSON :> Resource (Post Message JSON)
         )
       )
       :<|> "stateful" := "stateful" :/ (
              "increment" := "increment" :/ Resource (Post Int JSON)
         :<|> "read" := "read" :/ Resource (Get Int JSON)
       )
       -- we don't care what the return value gets encoded as
       :<|> "multiparse" := (
              "multiparse" :/ ReqBody ParseMethodDetect (JSON :<|> MagicContentType) :> Resource (Post ParseMethodDetect JSON)
       )

site :: Proxy Site
site = Proxy

type Handler m = ExceptT HTTPError m

messages :: Array Message
messages = Message <$> [ { id: 1, content: "Hello", unread: true }
                       , { id: 2, content: "Hi", unread: true }
                       , { id: 3, content: "How are you today?", unread: false }
                       ]

messageHasContent :: String -> Message -> Boolean
messageHasContent c (Message { content }) = String.contains (Pattern $ String.toLower c) (String.toLower content)

messageHasId :: Int -> Message -> Boolean
messageHasId expected (Message { id }) = id == expected

messageIsUnread :: Message -> Boolean
messageIsUnread (Message { unread }) = unread

resources
  :: forall m
   . MonadEffect m
  => { default :: { "GET" :: Handler m Default }
     , admin :: String -> { "GET" :: Handler m Admin }
     , api ::
       { messages ::
         { messages :: Array String -> Maybe PathBoolean -> { "GET" :: Handler m (Array Message) }
         , messageById :: Int -> { "GET" :: Handler m Message }
         , messagesById :: Array Int -> { "GET" :: Handler m (Array Message) }
         , newMessage :: Message -> { "POST" :: Handler m Message }
         }
       }
    , stateful :: 
      { increment :: { "POST" :: Handler m Int }
      , read :: { "GET" :: Handler m Int }
      }
    , multiparse :: ParseMethodDetect -> { "POST" :: Handler m ParseMethodDetect }
    }
resources =
  { default: { "GET": pure Default }
  , admin: \username -> { "GET": pure (Admin username) }
  , api:
    { messages:
      { messages: \content -> map (un PathBoolean) >>> \unread ->
          { "GET":
              messages
                # filter ( case unread of
                             Just true -> messageIsUnread
                             Just false -> not <<< messageIsUnread
                             Nothing -> const true
                         )
                # filter ( case content of
                             [] -> const true
                             cs -> foldr (||) (const false) (messageHasContent <$> cs)
                         )
                # pure
          }
      , messageById: \id ->
          { "GET":
              case find (messageHasId id) messages of
                Just message ->
                  pure message
                Nothing ->
                  throwError error404 { details = Just ("No message has ID " <> show id <> ".") }
          }
      , messagesById: \ids -> { "GET": pure $ filter (foldr (||) (const false) (messageHasId <$> ids)) messages }
      , newMessage: \message -> { "POST": pure message }
      }
    }
  , stateful:
    { increment: { "POST": liftEffect incrementStatefulVariable }
    , read: { "GET": liftEffect getStatefulVariable }
    }
  , multiparse: \p -> { "POST": pure p }
  }

-- Don't want to deal with AVar and converting the test Site into a ReaderT, so here's a lovely hack
foreign import incrementStatefulVariable :: Effect Int
foreign import getStatefulVariable :: Effect Int