module Nodetrout.Content where

import Prelude
import Control.Alternative ((<|>))
import Control.Monad.Except (ExceptT, throwError)
import Data.Array (elem, elemIndex)
import Data.List.NonEmpty (NonEmptyList, find, head, reverse, sortBy)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, textHTML)
import Data.String (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst)
import Foreign.Object (lookup)
import Network.HTTP (status406)
import Nodetrout.Context (Context)
import Nodetrout.Error (HTTPError(..))

data Acceptable
  = Required (Array MediaType)
  | Preferred (Array MediaType)
  | Anything

negotiate
  :: forall m content
   . Monad m
  => Context
  -> NonEmptyList (Tuple MediaType content)
  -> ExceptT HTTPError m (Tuple MediaType content)
negotiate context available = do
  acceptable <- getAcceptable context
  case (selectContent acceptable available) of
    Nothing ->
      throwError $ HTTPError { status: status406, details: Just "Content not available in requested format." }
    Just content ->
      pure content

getAcceptable
  :: forall m
   . Monad m
  => Context
  -> ExceptT HTTPError m Acceptable
getAcceptable { headers } =
  case (lookup "Accept" headers <|> lookup "accept" headers) of
    Nothing ->
      pure Anything
    Just "*/*" ->
      pure Anything
    Just header ->
      let
        values = trim <$> split (Pattern ",") header
        required = not $ "*/*" `elem` values
        eitherMimeTypes = values # traverse \v ->
          case v of
            "application/json" -> Right applicationJSON
            "text/html" -> Right textHTML
            unsupported -> Left unsupported
      in
        case eitherMimeTypes of
          Left unsupported ->
            throwError $ HTTPError { status: status406, details: Just $ "Unsupported media type " <> unsupported }
          Right mimeTypes ->
            pure $
              if required then
                Required mimeTypes
              else
                Preferred mimeTypes


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
