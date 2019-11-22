-- | This module contains various request-processing functionality that would
-- | otherwise clutter the `Router` module and increase its coupling to the Node
-- | server (bad for testability).
module Nodetrout.Internal.Request
  ( Request(..)
  , headerValue
  , method
  , path
  , queryParamValue
  , queryParamValues
  , removePath
  , stringBody
  , unconsPath
  ) where

import Prelude
import Data.Array (catMaybes, filter, head, uncons)
import Data.Either (Either)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode) as FUE
import Data.HTTP.Method (CustomMethod, Method)
import Data.HTTP.Method (fromString) as Method
import Data.Newtype (class Newtype, un)
import Data.String (joinWith, split, toLower) as String
import Data.String.CodeUnits (drop, dropWhile, takeWhile) as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Foreign.Object (Object, toArrayWithKey)

-- | A `Request` contains the request method, URL, and headers, as well as an
-- | `Aff` that reads the request body, which are used in the routing logic.
-- | Since this library is only interested in building a Node server, this
-- | abstraction really only exists to help test routing logic.
newtype Request = Request
  { method :: String
  , url :: String
  , headers :: Object String
  , stringBody :: Aff (Maybe String)
  }

derive instance newtypeRequest :: Newtype Request _

-- | Gets and parses the request method.
method :: Request -> Either Method CustomMethod
method = Method.fromString <<< _.method <<< un Request

-- | Gets and parses the request path (URL).
path :: Request -> Array String
path = filter (_ /= "") <<< String.split (Pattern "/") <<< String.takeWhile (_ /= '?') <<< _.url <<< un Request

-- | Gets and parses the request query string parameters.
query :: Request -> Array (Tuple String (Maybe String))
query (Request { url }) =
  fromMaybe [] $ un FormURLEncoded <$> FUE.decode (String.drop 1 $ String.dropWhile (_ /= '?') url)

-- | Gets the request headers.
headers :: Request -> Object String
headers = _.headers <<< un Request

-- | Gets the request body as a string.
stringBody :: Request -> Aff (Maybe String)
stringBody = _.stringBody <<< un Request

-- | Replaces the request path. Useful for scoping when routing the request.
replacePath :: Array String -> Request -> Request
replacePath p (Request r) =
  let
    queryString = String.drop 1 $ String.dropWhile (_ /= '?') r.url
  in
    Request $ r { url = "/" <> String.joinWith "/" p <> "?" <> queryString }

-- | Gets the next segment from the request path if available and returns it
-- | along with a scoped request. Useful when routing the request.
unconsPath :: Request -> Tuple (Maybe String) Request
unconsPath request = case uncons $ path request of
  Just { head, tail } ->
    Tuple (Just head) $ replacePath tail request
  Nothing ->
    Tuple Nothing request

-- | Captures the remaining segments from the request path and returns them along
-- | with a scoped request. Useful when routing the request.
removePath :: Request -> Tuple (Array String) Request
removePath request = Tuple (path request) $ replacePath [] request

-- | Attempts to get the value from a request header.
headerValue :: String -> Request -> Maybe String
headerValue name =
  map snd <<< find ((_ == String.toLower name) <<< String.toLower <<< fst) <<< toArrayWithKey Tuple <<< headers

-- | Gets all values from the query string matching the specified label (key).
-- | For example, given a label of `foo` and a request with a query string of
-- | `?foo=bar&foo=baz`, this would return an array `["bar", "baz"]`.
queryParamValues :: String -> Request -> Array String
queryParamValues label = catMaybes <<< map snd <<< filter (eq label <<< fst) <<< query

-- | Gets the first value from the query string matching the specified label
-- | (key). For example, given a label of `foo` and a request with a query string
-- | of `?foo=bar&foo=baz`, this would return a string `"bar"`.
queryParamValue :: String -> Request -> Maybe String
queryParamValue label = head <<< queryParamValues label
