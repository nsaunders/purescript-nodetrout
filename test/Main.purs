module Test.Main where

import Prelude
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Foreign.Object (empty, insert) as FO
import Nodetrout.Error (HTTPError)
import Nodetrout.Request (Request(..))
import Nodetrout.Router (route)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import TestSite (resources, site)

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
  , headers: FO.empty
  , readString: defer $ const (pure Nothing)
  }

processRequest :: forall m. Monad m => MonadAff m => RequestSpec -> m (Either HTTPError (Tuple MediaType String))
processRequest = runExceptT <<< route site resources <<< Request

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "content negotiation" do
    it "should provide the content in the client's preferred format when available" do
      result <- processRequest $ defaultRequest { headers = FO.insert "accept" "text/html" defaultRequest.headers }
      case result of
        Left _ ->
          fail "Request failed unexpectedly."
        Right (Tuple mediaType content) -> do
          mediaType `shouldEqual` textHTML
          content `shouldEqual` "<h1>Home Page</h1>"
