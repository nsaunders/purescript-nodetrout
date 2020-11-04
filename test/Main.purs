module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Array (filter)
import Data.ByteString (ByteString, fromUTF8, toUTF8)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (Object)
import Foreign.Object (insert, singleton) as FO
import Node.Stream as Stream
import Nodetrout.Internal.Content (ResponseWriter, writeResponse)
import Nodetrout.Internal.Error (HTTPError)
import Nodetrout.Internal.Request (Request(..))
import Nodetrout.Internal.Router (route)
import Test.Site (Default(..), ParseMethodDetect(..), magicMimeType, magicMimeTypeRenderString, messageHasContent, messageHasId, messageIsUnread, messages, resources, site)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

foreign import mkStreamBuffer :: forall m a. (a -> m a) -> m Stream.Duplex
foreign import getStreamBufferContents :: forall m a. (a -> m a) -> Stream.Duplex -> m ByteString

type RequestSpec =
  { method :: String
  , url :: String
  , headers :: Object String
  , bytestringBody :: Aff (Maybe ByteString)
  } 

defaultRequest :: RequestSpec
defaultRequest =
  { method: "GET"
  , url: "/"
  , headers: FO.singleton "accept" "*/*"
  , bytestringBody: pure Nothing
  }

captureResponse :: forall m. MonadEffect m => ResponseWriter -> m ByteString
captureResponse writer = do
  streamBuffer <- mkStreamBuffer pure
  liftEffect $ writeResponse streamBuffer writer
  getStreamBufferContents pure streamBuffer

processRequest :: forall m. Monad m => MonadAff m => RequestSpec -> m (Either HTTPError (Tuple MediaType String))
processRequest r = map (map fromUTF8) <$> processRequest' r

processRequest' :: forall m. Monad m => MonadAff m => RequestSpec -> m (Either HTTPError (Tuple MediaType ByteString))
processRequest' = runExceptT <<< (\r -> (route site resources r 0) >>= (\(Tuple mt resp) -> Tuple mt <$> captureResponse resp)) <<< Request

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "path segment handling" do
    it "should parse a path segment and provide it as an argument to the handler" do
      result <- processRequest $ defaultRequest { url = "/api/messages/2" }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) ->
          content `shouldEqual` (stringify $ encodeJson $ find (messageHasId 2) messages)
    it "should parse multiple path segments and pass them as an array to the handler" do
      result <- processRequest $ defaultRequest { url = "/api/messages/1/2" }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) ->
          content `shouldEqual` (stringify $ encodeJson $ filter (\m -> messageHasId 1 m || messageHasId 2 m) messages)
  describe "query string handling" do
    it "should parse a single query parameter and provide it as an argument to the handler" do
      result <- processRequest $ defaultRequest { url = "/api/messages?unread=true" }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) ->
          content `shouldEqual` (stringify $ encodeJson $ filter messageIsUnread messages)
    it "should parse multiple query parameters with the same label and pass them as an array to the handler" do
      result <- processRequest $ defaultRequest { url = "/api/messages?content=i&content=llo" }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) ->
          content
          `shouldEqual`
          (stringify $ encodeJson $ filter (\m -> messageHasContent "i" m || messageHasContent "llo" m) messages)
  describe "header handling" do
    it "should parse the specified header and provide it as an argument to the handler" do
      result <- processRequest $ defaultRequest
                  { url = "/admin"
                  , headers = FO.insert "Authorization" "fake_user" defaultRequest.headers
                  }
      case result of
        Left _ ->
          fail "Request failed unexpectedly."
        Right (Tuple _ content) ->
          content `shouldEqual` "<h1>fake_user</h1>"
  describe "request body processing" do
    it "should parse the expected request body and provide it as an argument to the handler" do
      let reqBody = stringify $ encodeJson { id: 4, content: "Greetings", unread: true }
      result <- processRequest $ defaultRequest
                  { method = "POST"
                  , url = "/api/messages"
                  , bytestringBody = pure $ Just (toUTF8 reqBody)
                  }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) -> do
          content `shouldEqual` reqBody
    it "should determine how to best parse the request body from the Content-Type request header when it's JSON" do
      let reqBody = stringify $ encodeJson { }
          encodedDetectedParseMethod = stringify $ encodeJson ParsedJson
      result <- processRequest $ defaultRequest
                  { method = "POST"
                  , url = "/multiparse"
                  , headers = FO.insert "content-type" "application/json" defaultRequest.headers
                  , bytestringBody = pure $ Just (toUTF8 reqBody)
                  }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) -> do
          content `shouldEqual` encodedDetectedParseMethod
    it "should determine how to best parse the request body from the Content-Type request header when it's something else" do
      let reqBody = stringify $ encodeJson { }
          encodedDetectedParseMethod = stringify $ encodeJson ParsedMagic
      result <- processRequest $ defaultRequest
                  { method = "POST"
                  , url = "/multiparse"
                  , headers = FO.insert "content-type" "application/magic" defaultRequest.headers
                  , bytestringBody = pure $ Just (toUTF8 reqBody)
                  }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple _ content) -> do
          content `shouldEqual` encodedDetectedParseMethod
    it "should determine how to best parse the request body when the Content-Type header is unspecified, taking the first successful parser" do
        let encodedParsedJson = stringify $ encodeJson ParsedJson
            encodedParsedMagic = stringify $ encodeJson ParsedMagic
            mkReq body = defaultRequest { method = "POST"
                                        , url = "/multiparse"
                                        , bytestringBody = pure $ Just (toUTF8 body)
                                        }
        processRequest (mkReq "garbage") >>= case _ of
          Left error ->
            fail $ "Request failed unexpectedly: " <> show error
          Right (Tuple _ content) -> do
            content `shouldEqual` encodedParsedMagic

        processRequest (mkReq "{}") >>= case _ of
          Left error ->
            fail $ "Request failed unexpectedly: " <> show error
          Right (Tuple _ content) -> do
            content `shouldEqual` encodedParsedJson

  describe "content negotiation" do
    it "should deliver the content in the client's preferred format when available" do
      result <- processRequest $ defaultRequest { headers = FO.insert "accept" "text/html" defaultRequest.headers }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple mediaType content) -> do
          mediaType `shouldEqual` textHTML
          content `shouldEqual` "<h1>Home Page</h1>"
    it "should be able to handle MIME types other than JSON and HTML" do
      result <- processRequest $ defaultRequest { headers = FO.insert "accept" "application/magic" defaultRequest.headers }
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple mediaType content) -> do
          mediaType `shouldEqual` magicMimeType
          content `shouldEqual` magicMimeTypeRenderString
    it "should deliver the content in the server's default format when the client will accept any content" do
      result <- processRequest defaultRequest
      case result of
        Left error ->
          fail $ "Request failed unexpectedly: " <> show error
        Right (Tuple mediaType content) -> do
          mediaType `shouldEqual` applicationJSON
          content `shouldEqual` (stringify $ encodeJson Default)

    let successfulStatefulReadRequest = do
          result <- processRequest $ defaultRequest { url = "/stateful/read" }
          case result of
            Left err-> throwError $ error $ "Request failed unexpectedly: " <> show err
            Right (Tuple mediaType content) -> do
              mediaType `shouldEqual` applicationJSON
              case decodeJson =<< jsonParser content of
                Left err -> throwError $ error $ "Request failed unexpectedly: " <> show err
                Right res -> pure res
    let defaultHeaders = defaultRequest.headers
        mkIncrementRequest acceptHeader = 
          defaultRequest { method = "POST" 
                         , url = "/stateful/increment"
                         , headers = fromMaybe defaultHeaders 
                              (flip (FO.insert "accept") defaultHeaders <$> acceptHeader)
                         }
        testStatefulIncrement acceptHeader = do
          initialState :: Int <- successfulStatefulReadRequest
          modifyResult <- processRequest (mkIncrementRequest acceptHeader)
          case modifyResult of
            Left err -> fail $ "Request failed unexpectedly: " <> show err
            Right (Tuple mediaType content) -> do
              mediaType `shouldEqual` applicationJSON
              case decodeJson =<< jsonParser content of
                Left err -> throwError $ error $ "Decoding response failed unexpectedly: " <> show err
                Right returnedState -> do
                  returnedState `shouldEqual` initialState
                  finalState <- successfulStatefulReadRequest
                  finalState `shouldEqual` (returnedState + 1)

    it "should process the request if the client supplies no Accept" $
      testStatefulIncrement Nothing
    it "should process the request if the client supplies an Accept that we can handle" $
      testStatefulIncrement (Just "application/json")

    it "should NOT process the request if the client can't accept the returned value" do
      initialState :: Int <- successfulStatefulReadRequest
      modifyResult <- processRequest $ mkIncrementRequest (Just "absolutely/unacceptable")
      case modifyResult of
        Right res -> fail $ "Request succeeded unexpectedly with returned value: " <> show res
        Left _ -> do
          finalState <- successfulStatefulReadRequest
          initialState `shouldEqual` finalState
      
