module Nodetrout.Context where
  
import Prelude
import Data.Array (filter, head, tail)
import Data.Either (Either)
import Data.FormURLEncoded (FormURLEncoded(..), decode)
import Data.HTTP.Method (Method, CustomMethod)
import Data.HTTP.Method (fromString) as Method
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.String.Common (split)
import Data.String.CodeUnits (drop, indexOf, take)
import Data.String.Pattern (Pattern(..))
import Node.HTTP (Request, requestMethod, requestURL)

type Context =
  { method :: Either Method CustomMethod
  , path :: Array String
  , query :: Array (Tuple String (Maybe String))
  }

fromRequest :: Request -> Context
fromRequest req =
  let
    method = Method.fromString $ requestMethod req
    url = requestURL req
    queryPosition = indexOf (Pattern "?") url
    path = filter (_ /= "") $ split (Pattern "/") $ fromMaybe url (flip take url <$> queryPosition)
    query = fromMaybe [] $ un FormURLEncoded <$> (decode =<< (flip drop url <$> queryPosition))
  in
    { method, path, query }

shiftPath :: Context -> Tuple (Maybe String) Context
shiftPath { method, path, query } = Tuple (head path) { method, path: fromMaybe [] $ tail path, query }
