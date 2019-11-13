module Nodetrout.Router where
  
import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (null)
import Data.Either (Either(..), note)
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
import Nodetrout.Error (HTTPError, _errorDetails, _errorPriority, error400, error404, error405)
import Nodetrout.Request (Request)
import Nodetrout.Request
  ( headerValue
  , method
  , path
  , queryParamValue
  , queryParamValues
  , removePath
  , stringBody
  , unconsPath
  ) as Request
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get) as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Trout
  ( type (:<|>)
  , type (:>)
  , type (:=)
  , Capture
  , CaptureAll
  , Header
  , Lit
  , QueryParam
  , QueryParams
  , ReqBody
  , Resource
  )
import Type.Trout (Method) as Trout
import Type.Trout.ContentType (class AllMimeRender, class MimeParse, allMimeRender, mimeParse)
import Type.Trout.Header (class FromHeader, fromHeader)
import Type.Trout.PathPiece (class FromPathPiece, fromPathPiece)

type Depth = Int

class Router layout handlers m result | layout -> handlers, layout -> result where
  route :: Proxy layout -> handlers -> Request -> Depth -> ExceptT HTTPError m result

instance routerAltNamed ::
  ( Monad m
  , Router layout handler m result
  , Router otherLayout (Record otherHandlers) m result
  , IsSymbol name
  , Row.Cons name handler otherHandlers handlers
  , Row.Lacks name otherHandlers
  ) => Router (name := layout :<|> otherLayout) (Record handlers) m result where
  route _ handlers request depth = do
    eitherResult1 <- lift $ runExceptT $ route (Proxy :: Proxy layout) (Record.get name handlers) request depth
    case eitherResult1 of
      Right result ->
        pure result
      Left error1 -> do
        eitherResult2 <- lift $ runExceptT $ route (Proxy :: Proxy otherLayout) (Record.delete name handlers) request depth
        case eitherResult2 of
          Right result ->
            pure result
          Left error2 ->
            throwError $ Error.select error1 error2
    where
      name = SProxy :: SProxy name

instance routerNamed ::
  ( Monad m
  , Router layout handler m result
  , IsSymbol name
  , Row.Cons name handler () handlers
  ) => Router (name := layout) (Record handlers) m result where
  route _ handlers request depth = route (Proxy :: Proxy layout) (Record.get (SProxy :: SProxy name) handlers) request depth

instance routerLit ::
  ( Monad m
  , Router layout handlers m result
  , IsSymbol segment
  ) => Router (Lit segment :> layout) handlers m result where
  route _ handlers request depth =
    case Request.unconsPath request of
      Tuple (Just head) scopedRequest | head == reflectSymbol (SProxy :: SProxy segment) ->
        route (Proxy :: Proxy layout) handlers scopedRequest (depth + 1)
      _ ->
        throwError $ error404
                   # _errorPriority .~ depth

instance routerCapture ::
  ( Monad m
  , Router layout handlers m result
  , FromPathPiece value
  ) => Router (Capture label value :> layout) (value -> handlers) m result where
  route _ handlers request depth =
    case Request.unconsPath request of
      Tuple (Just head) scopedRequest ->
        case fromPathPiece head of
          Right value ->
            route (Proxy :: Proxy layout) (handlers value) scopedRequest $ depth + 1
          Left _ ->
            throwError $ error404
                       # _errorPriority .~ depth
      _ ->
        throwError $ error404
                   # _errorPriority .~ depth

instance routerCaptureAll ::
  ( Monad m
  , Router layout handlers m result
  , FromPathPiece value
  ) => Router (CaptureAll label value :> layout) (Array value -> handlers) m result where
  route _ handlers request depth =
    let
      (Tuple path scopedRequest) = Request.removePath request
    in
      case traverse fromPathPiece path of
        Right value ->
          route (Proxy :: Proxy layout) (handlers value) scopedRequest $ depth + 1
        Left _ ->
          throwError $ error404
                     # _errorPriority .~ depth

instance routerQueryParam ::
  ( Monad m
  , Router layout handlers m result
  , IsSymbol label
  , FromPathPiece value
  ) => Router (QueryParam label value :> layout) (Maybe value -> handlers) m result where
  route _ handlers request depth =
    let
      label = reflectSymbol (SProxy :: SProxy label)
    in
      case Request.queryParamValue label request of
        Just paramValue ->
          case fromPathPiece paramValue of
            Right value ->
              route (Proxy :: Proxy layout) (handlers $ Just value) request $ depth + 1
            Left _ ->
              throwError $ error400
                         # _errorDetails .~ Just ("Invalid value for query parameter " <> label)
                         # _errorPriority .~ depth
        Nothing ->
          route (Proxy :: Proxy layout) (handlers Nothing) request $ depth + 1

instance routerQueryParams ::
  ( Monad m
  , Router layout handlers m result
  , IsSymbol label
  , FromPathPiece value
  ) => Router (QueryParams label value :> layout) (Array value -> handlers) m result where
  route _ handlers request depth =
    let
      label = reflectSymbol (SProxy :: SProxy label)
    in
      case traverse fromPathPiece $ Request.queryParamValues label request of
        Right values ->
          route (Proxy :: Proxy layout) (handlers values) request $ depth + 1
        Left _ ->
          throwError $ error400
                     # _errorDetails .~ Just ("Invalid value for query parameter " <> label)
                     # _errorPriority .~ depth

instance routerHeader ::
  ( Monad m
  , Router layout handlers m result
  , IsSymbol name
  , FromHeader value
  ) => Router (Header name value :> layout) (value -> handlers) m result where
  route _ handlers request depth =
    let
      name = reflectSymbol (SProxy :: SProxy name)
    in
      case (note ("Missing value") (Request.headerValue name request) >>= fromHeader) of
        Right value ->
          route (Proxy :: Proxy layout) (handlers value) request $ depth + 1
        Left error ->
          throwError $ error400
                     # _errorDetails .~ Just ("Header " <> name <> ": " <> error)
                     # _errorPriority .~ depth

instance routerReqBody ::
  ( Monad m
  , MonadAff m
  , Router layout handlers m result
  , MimeParse String contentType parsed
  ) => Router (ReqBody parsed contentType :> layout) (parsed -> handlers) m result where
  route _ handlers request depth =
    liftAff (Request.stringBody request) >>= case _ of
      Just body ->
        case mimeParse (Proxy :: Proxy contentType) body of
          Right parsed ->
            route (Proxy :: Proxy layout) (handlers parsed) request $ depth + 1
          Left e ->
            throwError $ error400
                       # _errorDetails .~ Just ("The request body was not in the expected format: " <> e)
                       # _errorPriority .~ depth
      Nothing ->
        throwError $ error400
                   # _errorDetails .~ Just "A request body is required, but none was provided."
                   # _errorPriority .~ depth

instance routerMethod ::
  ( Monad m
  , IsSymbol method
  , AllMimeRender body contentTypes rendered
  , Row.Cons method (ExceptT HTTPError m body) handlers' handlers
  ) => Router (Trout.Method method body contentTypes) (Record handlers) m (Tuple MediaType rendered) where
  route layout handlers request depth = do
    let method = SProxy :: SProxy method
    when (not $ null $ Request.path request)
      $ throwError (error404 # _errorPriority .~ depth)
    when (Request.method request /= Method.fromString (reflectSymbol method))
      $ throwError (error405 # _errorPriority .~ depth)
    body <- Record.get method handlers
    content <- Content.negotiate request $ allMimeRender (Proxy :: Proxy contentTypes) body
    pure content

instance routerResource ::
  ( Monad m
  , Router layout handlers m result
  ) => Router (Resource layout) handlers m result where
  route _ = route (Proxy :: Proxy layout)
