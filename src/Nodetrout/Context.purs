module Nodetrout.Context where
  
import Prelude
import Data.Array (filter)
import Data.Either (Either)
import Data.FormURLEncoded (FormURLEncoded(..), decode)
import Data.HTTP.Method (Method, CustomMethod)
import Data.HTTP.Method (fromString) as Method
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (un)
import Data.Tuple (Tuple)
import Data.String.Common (split)
import Data.String.CodeUnits (drop, indexOf, take)
import Data.String.Pattern (Pattern(..))
import Foreign.Object (Object)
import Node.HTTP (Request, requestAsStream, requestHeaders, requestMethod, requestURL)
import Node.Stream (Readable)

type Context =
  { method :: Either Method CustomMethod
  , path :: Array String
  , query :: Array (Tuple String (Maybe String))
  , headers :: Object String
  , body :: Readable ()
  }

fromRequest :: Request -> Context
fromRequest req =
  let
    method = Method.fromString $ requestMethod req
    url = requestURL req
    queryPosition = indexOf (Pattern "?") url
    path = filter (_ /= "") $ split (Pattern "/") $ fromMaybe url (flip take url <$> queryPosition)
    query = fromMaybe [] $ un FormURLEncoded <$> (decode =<< ((flip drop url <<< (_ + 1)) <$> queryPosition))
    headers = requestHeaders req
    body = requestAsStream req
  in
    { method, path, query, headers, body }
