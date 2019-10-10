module Nodetrout.Router where
  
import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.HTTP.Method (fromString) as Method
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Nodetrout.Content (negotiate) as Content
import Nodetrout.Error (select) as Error
import Nodetrout.Error (HTTPError, _errorDetails, error400, error404, error405)
import Nodetrout.Request (Request)
import Nodetrout.Request
  ( method
  , path
  , queryParamValue
  , queryParamValues
  , readString
  , removePath
  , unconsPath
  ) as Request
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get) as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>), type (:>), type (:=), Capture, CaptureAll, Lit, QueryParam, QueryParams, ReqBody, Resource)
import Type.Trout (Method) as Trout
import Type.Trout.ContentType (class AllMimeRender, class MimeParse, allMimeRender, mimeParse)
import Type.Trout.PathPiece (class FromPathPiece, fromPathPiece)

class Router layout handlers result | layout -> handlers, layout -> result where
  route :: Proxy layout -> handlers -> Request -> result

instance routerAltNamed ::
  ( Monad m
  , Router layout handler (ExceptT HTTPError m result)
  , Router otherLayout (Record otherHandlers) (ExceptT HTTPError m result)
  , IsSymbol name
  , Row.Cons name handler otherHandlers handlers
  , Row.Lacks name otherHandlers
  ) => Router (name := layout :<|> otherLayout) (Record handlers) (ExceptT HTTPError m result) where
  route _ handlers request = do
    eitherResult1 <- lift $ runExceptT $ route (Proxy :: Proxy layout) (Record.get name handlers) request
    case eitherResult1 of
      Right result ->
        pure result
      Left error1 -> do
        eitherResult2 <- lift $ runExceptT $ route (Proxy :: Proxy otherLayout) (Record.delete name handlers) request
        case eitherResult2 of
          Right result ->
            pure result
          Left error2 ->
            throwError $ Error.select error1 error2
    where
      name = SProxy :: SProxy name

instance routerNamed ::
  ( Monad m
  , Router layout handler (ExceptT HTTPError m result)
  , IsSymbol name
  , Row.Cons name handler () handlers
  ) => Router (name := layout) (Record handlers) (ExceptT HTTPError m result) where
  route _ handlers = route (Proxy :: Proxy layout) (Record.get (SProxy :: SProxy name) handlers)

instance routerLit ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m result)
  , IsSymbol segment
  ) => Router (Lit segment :> layout) handlers (ExceptT HTTPError m result) where
  route _ handlers request =
    case Request.unconsPath request of
      Tuple (Just head) scopedRequest | head == reflectSymbol (SProxy :: SProxy segment) ->
        route (Proxy :: Proxy layout) handlers scopedRequest
      _ ->
        throwError error404

instance routerCapture ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , FromPathPiece value
  ) => Router (Capture label value :> layout) (value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers request =
    case Request.unconsPath request of
      Tuple (Just head) scopedRequest ->
        case fromPathPiece head of
          Right value ->
            route (Proxy :: Proxy layout) (handlers value) scopedRequest
          Left _ ->
            throwError error404
      _ ->
        throwError error404

instance routerCaptureAll ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , FromPathPiece value
  ) => Router (CaptureAll label value :> layout) (Array value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers request =
    let
      (Tuple path scopedRequest) = Request.removePath request
    in
      case traverse fromPathPiece path of
        Right value ->
          route (Proxy :: Proxy layout) (handlers value) scopedRequest
        Left _ ->
          throwError error404

instance routerQueryParam ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , IsSymbol label
  , FromPathPiece value
  ) => Router (QueryParam label value :> layout) (Maybe value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers request =
    let
      label = reflectSymbol (SProxy :: SProxy label)
    in
      case Request.queryParamValue label request of
        Just paramValue ->
          case fromPathPiece paramValue of
            Right value ->
              route (Proxy :: Proxy layout) (handlers $ Just value) request
            Left _ ->
              throwError $ error400 # _errorDetails .~ Just ("Invalid value for query parameter " <> label)
        Nothing ->
          route (Proxy :: Proxy layout) (handlers Nothing) request

instance routerQueryParams ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , IsSymbol label
  , FromPathPiece value
  ) => Router (QueryParams label value :> layout) (Array value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers request =
    let
      label = reflectSymbol (SProxy :: SProxy label)
    in
      case traverse fromPathPiece $ Request.queryParamValues label request of
        Right values ->
          route (Proxy :: Proxy layout) (handlers values) request
        Left _ ->
          throwError $ error400 # _errorDetails .~ Just ("Invalid value for query parameter " <> label)

instance routerReqBody ::
  ( Monad m
  , MonadAff m
  , Router layout handlers (ExceptT HTTPError m next)
  , MimeParse String contentType parsed
  ) => Router (ReqBody parsed contentType :> layout) (parsed -> handlers) (ExceptT HTTPError m next) where
  route _ handlers request =
    liftAff (Request.readString request) >>= case _ of
      Just body ->
        case mimeParse (Proxy :: Proxy contentType) body of
          Right parsed ->
            route (Proxy :: Proxy layout) (handlers parsed) request
          Left _ ->
            throwError $ error400 # _errorDetails .~ Just "The request body was not in the expected format."
      Nothing ->
        throwError $ error400 # _errorDetails .~ Just "A request body is required, but none was provided."

instance routerMethod ::
  ( Monad m
  , IsSymbol method
  , AllMimeRender body contentTypes rendered
  , Row.Cons method (ExceptT HTTPError m body) handlers' handlers
  ) => Router (Trout.Method method body contentTypes) (Record handlers) (ExceptT HTTPError m (Tuple MediaType rendered)) where
  route layout handlers request = do
    let method = SProxy :: SProxy method
    when (not $ null $ Request.path request)
      $ throwError error404
    when (Request.method request /= Method.fromString (reflectSymbol method))
      $ throwError error405
    body <- Record.get method handlers
    content <- Content.negotiate request $ allMimeRender (Proxy :: Proxy contentTypes) body
    pure content

instance routerResource ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m result)
  ) => Router (Resource layout) handlers (ExceptT HTTPError m result) where
  route _ = route (Proxy :: Proxy layout)
