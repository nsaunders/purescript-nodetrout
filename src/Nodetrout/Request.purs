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
import Data.String (toLower, split) as String
import Data.String.CodeUnits (drop, dropWhile, takeWhile) as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref (modify_, new, read) as Ref
import Foreign.Object (Object, toArrayWithKey)
import Node.Buffer (concat, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (Request) as NH
import Node.HTTP (requestHeaders, requestMethod, requestAsStream, requestURL)
import Node.Stream (onData, onEnd) as Stream

newtype Request = Request
  { method :: Lazy (Either Method CustomMethod)
  , path :: Lazy (Array String)
  , query :: Lazy (Array (Tuple String (Maybe String)))
  , headers :: Object String
  , readToString :: Lazy (Aff (Maybe String))
  }

derive instance newtypeRequest :: Newtype Request _

fromNodeRequest :: NH.Request -> Request
fromNodeRequest req = Request
  { method: defer \_ -> Method.fromString $ requestMethod req
  , path: defer \_ -> String.split (Pattern "/") $ String.takeWhile (_ /= '?') $ String.drop 1 $ requestURL req
  , query: defer \_ ->
      fromMaybe [] $ un FormURLEncoded <$> FUE.decode (String.drop 1 $ String.dropWhile (_ /= '?') $ requestURL req)
  , headers: requestHeaders req
  , readToString: defer \_ ->
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
method = force <<< _.method <<< un Request

path :: Request -> Array String
path = force <<< _.path <<< un Request

query :: Request -> Array (Tuple String (Maybe String))
query = force <<< _.query <<< un Request

headers :: Request -> Object String
headers = _.headers <<< un Request

readToString :: Request -> Aff (Maybe String)
readToString = force <<< _.readToString <<< un Request

replacePath :: Array String -> Request -> Request
replacePath p (Request r) = Request $ r { path = defer $ const p }

unconsPath :: Request -> Tuple (Maybe String) Request
unconsPath request = case (uncons $ path request) of
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
