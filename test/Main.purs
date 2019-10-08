module Test.Main where

import Prelude
import Control.Monad.Except (runExceptT)
import Data.Argonaut (encodeJson, stringify)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Foreign.Object (insert, singleton) as FO
import Nodetrout.Error (HTTPError)
import Nodetrout.Request (Request(..))
import Nodetrout.Router (route)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import TestSite (Default(..), resources, site, unreadMessages) as Site

type RequestSpec =
  { method :: String
  , url :: String
  , headers :: Object String
  , readString :: Lazy (Aff (Maybe String))
  } 

defaultRequest :: RequestSpec
defaultRequest =
  { method: "GET"
  , url: "/"
  , headers: FO.singleton "accept" "*/*"
  , readString: defer $ const (pure Nothing)
  }

processRequest :: forall m. Monad m => MonadAff m => RequestSpec -> m (Either HTTPError (Tuple MediaType String))
processRequest = runExceptT <<< route Site.site Site.resources <<< Request

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "content negotiation" do
    it "should deliver the content in the client's preferred format when available" do
      result <- processRequest $ defaultRequest { headers = FO.insert "accept" "text/html" defaultRequest.headers }
      case result of
        Left _ ->
          fail "Request failed unexpectedly."
        Right (Tuple mediaType content) -> do
          mediaType `shouldEqual` textHTML
          content `shouldEqual` "<h1>Home Page</h1>"
    it "should deliver the content in the server's default format when the client will accept any content" do
      result <- processRequest defaultRequest
      case result of
        Left _ ->
          fail "Request failed unexpectedly."
        Right (Tuple mediaType content) -> do
          mediaType `shouldEqual` applicationJSON
          content `shouldEqual` (stringify $ encodeJson Site.Default)
    it "should parse a single query parameter and provide it as an argument to the handler" do
      result <- processRequest $ defaultRequest { url = "/api/messages?unread=true" }
      case result of
        Left _ ->
          fail "Request failed unexpectedly."
        Right (Tuple _ content) ->
          content `shouldEqual` (stringify $ encodeJson Site.unreadMessages)
