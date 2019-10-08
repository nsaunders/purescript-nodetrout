module TestSite where

import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject)
import Data.Array (filter)
import Data.Either (Either(Left))
import Data.Foldable (find, foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.String (contains, toLower) as String
import Data.String.Pattern (Pattern(..))
import Network.HTTP (status404)
import Nodetrout.Error (HTTPError(..))
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
import Type.Trout.PathPiece (class FromPathPiece)

data Default = Default

instance encodeJsonDefault :: EncodeJson Default where
  encodeJson _ = jsonEmptyObject

instance encodeHTMLDefault :: EncodeHTML Default where
  encodeHTML _ = h1 $ text "Home Page"

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

type Site = "default" := Resource (Get Default (JSON :<|> HTML))
       :<|> "messages" := "api" :/ "messages" :/ QueryParams "content" String :> QueryParam "unread" PathBoolean :> Resource (Get (Array Message) JSON)
       :<|> "messageById" := "api" :/ "messages" :/ Capture "id" Int :> Resource (Get Message JSON)
       :<|> "messagesById" := "api" :/ "messages" :/ CaptureAll "id" Int :> Resource (Get (Array Message) JSON)
       :<|> "newMessage" := "api" :/ "messages" :/ ReqBody Message JSON :> Resource (Post Message JSON)

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

messageMatchesId :: Int -> Message -> Boolean
messageMatchesId expected (Message { id }) = id == expected

messageUnread :: Message -> Boolean
messageUnread (Message { unread }) = unread

unreadMessages :: Array Message
unreadMessages = filter messageUnread messages

resources
  :: forall m
   . Monad m
  => { default :: { "GET" :: Handler m Default }
     , messages :: Array String -> Maybe PathBoolean -> { "GET" :: Handler m (Array Message) }
     , messageById :: Int -> { "GET" :: Handler m Message }
     , messagesById :: Array Int -> { "GET" :: Handler m (Array Message) }
     , newMessage :: Message -> { "POST" :: Handler m Message }
     }
resources =
  { default: { "GET": pure Default }
  , messages: \c -> map (un PathBoolean) >>> \u ->
      { "GET": pure $ filter (foldr (||) (eq (fromMaybe false u) <<< messageUnread) (messageHasContent <$> c)) messages }
  , messageById: \id ->
      { "GET":
          case find (messageMatchesId id) messages of
            Just message ->
              pure message
            Nothing ->
              throwError $ HTTPError { status: status404, details: Just $ "No message has ID " <> show id <> "." }
      }
  , messagesById: \ids -> { "GET": pure $ filter (foldr (||) (const false) (messageMatchesId <$> ids)) messages }
  , newMessage: \message -> { "POST": pure message }
  }
