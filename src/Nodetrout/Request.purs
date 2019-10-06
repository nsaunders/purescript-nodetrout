module Nodetrout.Request where

import Prelude
import Data.Array (catMaybes, cons, filter, head, uncons)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode) as FUE
import Data.HTTP.Method (CustomMethod, Method)
import Data.HTTP.Method (fromString) as Method
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype, un)
import Data.String (joinWith, split, toLower) as String
import Data.String.CodeUnits (drop, dropWhile, takeWhile) as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref (modify_, new, read) as Ref
import Foreign.Object (Object, toArrayWithKey)
import Foreign.Object (empty) as FO
import Node.Buffer (concat, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (Request) as NH
import Node.HTTP (requestHeaders, requestMethod, requestAsStream, requestURL)
import Node.Stream (onData, onEnd) as Stream

newtype Request = Request
  { method :: String
  , url :: String
  , headers :: Object String
  , readString :: Lazy (Aff (Maybe String))
  }

derive instance newtypeRequest :: Newtype Request _

fromNodeRequest :: NH.Request -> Request
fromNodeRequest req = Request
  { method: requestMethod req
  , url: requestURL req
  , headers: requestHeaders req
  , readString: defer \_ ->
      makeAff \done -> do
        chunks <- Ref.new []
        Stream.onData (requestAsStream req) \chunk -> Ref.modify_ (cons chunk) chunks
        Stream.onEnd (requestAsStream req) $ Ref.read chunks >>=
          case _ of
            [] ->
              done $ Right Nothing
            chx ->
              Buffer.concat chx >>= Buffer.toString UTF8 >>= Just >>> Right >>> done
        pure nonCanceler
  }

method :: Request -> Either Method CustomMethod
method = Method.fromString <<< _.method <<< un Request

path :: Request -> Array String
path = filter (_ /= "") <<< String.split (Pattern "/") <<< String.takeWhile (_ /= '?') <<< _.url <<< un Request

query :: Request -> Array (Tuple String (Maybe String))
query (Request { url }) =
  fromMaybe [] $ un FormURLEncoded <$> FUE.decode (String.drop 1 $ String.dropWhile (_ /= '?') url)

headers :: Request -> Object String
headers = _.headers <<< un Request

readString :: Request -> Aff (Maybe String)
readString = force <<< _.readString <<< un Request

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
