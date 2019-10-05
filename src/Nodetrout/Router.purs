module Nodetrout.Router where
  
import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, filter, null)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.HTTP.Method (fromString) as Method
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.HTTP (status400, status404, status405)
import Nodetrout.Content (negotiate) as Content
import Nodetrout.Error (select) as Error
import Nodetrout.Error (HTTPError(..))
import Nodetrout.Request (Request)
import Nodetrout.Request (method, path, query, readToString, removePath, unconsPath) as Request
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
        throwError $ HTTPError { status: status404, details: Nothing }

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
          Left error ->
            throwError $ HTTPError { status: status400, details: Just error }
      _ ->
        throwError $ HTTPError { status: status404, details: Nothing }

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
          throwError $ HTTPError { status: status404, details: Nothing }

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
      case join $ snd <$> (find ((_ == label) <<< fst) $ Request.query request) of
        Just param ->
          case fromPathPiece param of
            Right value ->
              route (Proxy :: Proxy layout) (handlers $ Just value) request
            Left _ ->
              throwError $ HTTPError { status: status400, details: Just $ "Invalid value for query parameter " <> label }
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
      case traverse fromPathPiece $ catMaybes $ map snd $ filter ((_ == label) <<< fst) $ Request.query request of
        Right values ->
          route (Proxy :: Proxy layout) (handlers values) request
        Left _ ->
          throwError $ HTTPError { status: status400, details: Just $ "Invalid value for query parameter " <> label }

instance routerReqBody ::
  ( Monad m
  , MonadAff m
  , Router layout handlers (ExceptT HTTPError m next)
  , MimeParse String contentType parsed
  ) => Router (ReqBody parsed contentType :> layout) (parsed -> handlers) (ExceptT HTTPError m next) where
  route _ handlers request =
    liftAff (Request.readToString request) >>= case _ of
      Just body ->
        case mimeParse (Proxy :: Proxy contentType) body of
          Right parsed ->
            route (Proxy :: Proxy layout) (handlers parsed) request
          Left _ ->
            throwError $ HTTPError { status: status400, details: Just "Could not parse the request body." }
      Nothing ->
        throwError $ HTTPError { status: status400, details: Just "A request body is required, but none was present." }

instance routerMethod ::
  ( Monad m
  , IsSymbol method
  , AllMimeRender body contentTypes rendered
  , Row.Cons method (ExceptT HTTPError m body) handlers' handlers
  ) => Router (Trout.Method method body contentTypes) (Record handlers) (ExceptT HTTPError m (Tuple MediaType rendered)) where
  route layout handlers request = do
    let method = SProxy :: SProxy method
    when (not $ null $ Request.path request)
      $ throwError $ HTTPError { status: status404, details: Nothing }
    when (Request.method request /= Method.fromString (reflectSymbol method))
      $ throwError $ HTTPError { status: status405, details: Nothing }
    body <- Record.get method handlers
    content <- Content.negotiate request $ allMimeRender (Proxy :: Proxy contentTypes) body
    pure content

instance routerResource ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m result)
  ) => Router (Resource layout) handlers (ExceptT HTTPError m result) where
  route _ = route (Proxy :: Proxy layout)
