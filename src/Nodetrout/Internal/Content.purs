-- | This module contains types and logic related to content negotiation.
module Nodetrout.Internal.Content
  ( class ResponseWritable
  , ResponseWriter
  , mkResponseWriter
  , negotiate
  , writeResponse
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except (ExceptT, throwError)
import Data.Array (catMaybes, elem, elemIndex, length)
import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmptyList, find, head, reverse, sortBy)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.String (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (Writable, write, writeString) as Stream
import Nodetrout.Internal.Error (HTTPError, error406)
import Nodetrout.Internal.Request (Request, headerValue, toUnparameterizedMediaType)

data Acceptable
  = Required (Array MediaType)
  | Preferred (Array MediaType)
  | Anything

-- | Attempts to return the available content that best matches the client's
-- | `Accept` header.
negotiate
  :: forall m body
   . Monad m
  => Request
  -> NonEmptyList (Tuple MediaType (body -> ResponseWriter))
  -> ExceptT HTTPError m body
  -> ExceptT HTTPError m (Tuple MediaType ResponseWriter)
negotiate request available runContent = do
  acceptable <- getAcceptable request
  case (selectContent acceptable available) of
    Nothing ->
      throwError error406 { details = Just "This content is not available in the requested format." }
    Just (Tuple mediaType renderContent) -> Tuple mediaType <$> (renderContent <$> runContent)

-- | Parses known media types from the client's `Accept` header.
getAcceptable
  :: forall m
   . Monad m
  => Request
  -> ExceptT HTTPError m Acceptable
getAcceptable =
  headerValue "accept" >>> case _ of
    Nothing ->
      pure Anything
    Just "*/*" ->
      pure Anything
    Just header ->
      let
        values = trim <$> split (Pattern ",") header
        acceptable = if not $ "*/*" `elem` values then Required else Preferred
        mimeTypes = catMaybes $ values <#> toUnparameterizedMediaType
      in
        if (length mimeTypes == 0)
          then throwError error406 { details = Just "The requested media types are unsupported." }
          else pure (acceptable mimeTypes)

-- | Selects from a list of content in various formats by what format is
-- | `Acceptable`.
selectContent
  :: forall body
   . Acceptable
  -> NonEmptyList (Tuple MediaType (body -> ResponseWriter))
  -> Maybe (Tuple MediaType (body -> ResponseWriter))
selectContent = case _ of
  Anything ->
    pure <<< head
  Required requested ->
    let
      sortWith f = sortBy \a b -> compare (f a) (f b)
    in
      find ((_ `elem` requested) <<< fst) <<< reverse <<< sortWith ((_ `elemIndex` requested) <<< fst)
  Preferred requested ->
    \available -> selectContent (Required requested) available <|> selectContent Anything available

-- | Specifies how to write `content` to a response stream.
class ResponseWritable content where
  writeResponse :: forall r. Stream.Writable r -> content -> Effect Unit

instance responseWritableString :: ResponseWritable String where
  writeResponse stream content = Stream.writeString stream UTF8 content (pure unit) *> pure unit
instance responseWritableByteString :: ResponseWritable ByteString where
  writeResponse stream content = Stream.write stream (ByteString.unsafeThaw content) (pure unit) *> pure unit

data ResponseWriter = ResponseWriter (forall r. Stream.Writable r -> Effect Unit)
mkResponseWriter :: forall w. ResponseWritable w => w -> ResponseWriter
mkResponseWriter content = ResponseWriter (\stream -> writeResponse stream content)

instance responseWritableResponseWriter :: ResponseWritable ResponseWriter where
  writeResponse stream (ResponseWriter w) = w stream