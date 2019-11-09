module Nodetrout.Request where

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

newtype Request = Request
  { method :: String
  , url :: String
  , headers :: Object String
  , stringBody :: Aff (Maybe String)
  }

derive instance newtypeRequest :: Newtype Request _

method :: Request -> Either Method CustomMethod
method = Method.fromString <<< _.method <<< un Request

path :: Request -> Array String
path = filter (_ /= "") <<< String.split (Pattern "/") <<< String.takeWhile (_ /= '?') <<< _.url <<< un Request

query :: Request -> Array (Tuple String (Maybe String))
query (Request { url }) =
  fromMaybe [] $ un FormURLEncoded <$> FUE.decode (String.drop 1 $ String.dropWhile (_ /= '?') url)

headers :: Request -> Object String
headers = _.headers <<< un Request

stringBody :: Request -> Aff (Maybe String)
stringBody = _.stringBody <<< un Request

replacePath :: Array String -> Request -> Request
replacePath p (Request r) =
  let
    queryString = String.drop 1 $ String.dropWhile (_ /= '?') r.url
  in
    Request $ r { url = "/" <> String.joinWith "/" p <> "?" <> queryString }

unconsPath :: Request -> Tuple (Maybe String) Request
unconsPath request = case uncons $ path request of
  Just { head, tail } ->
    Tuple (Just head) $ replacePath tail request
  Nothing ->
    Tuple Nothing request

removePath :: Request -> Tuple (Array String) Request
removePath request = Tuple (path request) $ replacePath [] request

headerValue :: String -> Request -> Maybe String
headerValue name =
  map snd <<< find ((_ == String.toLower name) <<< String.toLower <<< fst) <<< toArrayWithKey Tuple <<< headers

queryParamValues :: String -> Request -> Array String
queryParamValues label = catMaybes <<< map snd <<< filter (eq label <<< fst) <<< query

queryParamValue :: String -> Request -> Maybe String
queryParamValue label = head <<< queryParamValues label
