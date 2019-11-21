module Nodetrout.Internal.Content where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Except (ExceptT, throwError)
import Data.Array (catMaybes, elem, elemIndex, length)
import Data.List.NonEmpty (NonEmptyList, find, head, reverse, sortBy)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Data.String (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple, fst)
import Nodetrout.Internal.Error (HTTPError, error406)
import Nodetrout.Internal.Request (Request, headerValue)

data Acceptable
  = Required (Array MediaType)
  | Preferred (Array MediaType)
  | Anything

-- | Attempts to return the available content that best matches the client's
-- | `Accept` header.
negotiate
  :: forall m content
   . Monad m
  => Request
  -> NonEmptyList (Tuple MediaType content)
  -> ExceptT HTTPError m (Tuple MediaType content)
negotiate request available = do
  acceptable <- getAcceptable request
  case (selectContent acceptable available) of
    Nothing ->
      throwError error406 { details = Just "This content is not available in the requested format." }
    Just content ->
      pure content

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
        mimeTypes = catMaybes $ values <#> case _ of
          "application/json" -> Just applicationJSON
          "text/html" -> Just textHTML
          _ -> Nothing
      in
        if (length mimeTypes == 0)
          then throwError error406 { details = Just "The requested media types are unsupported." }
          else pure (acceptable mimeTypes)

-- | Selects from a list of content in various formats by what format is
-- | `Acceptable`.
selectContent
  :: forall content
   . Acceptable
  -> NonEmptyList (Tuple MediaType content)
  -> Maybe (Tuple MediaType content)
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
