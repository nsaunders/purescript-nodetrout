-- | This module contains the routing logic.
module Nodetrout.Internal.Router (class DeferredAllMimeRender, class Router, deferredMimeRenderers, route) where
  
import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (null)
import Data.Either (Either(..), note)
import Data.HTTP.Method (fromString) as Method
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Nodetrout.Internal.Content (negotiate) as Content
import Nodetrout.Internal.Error (HTTPError, error400, error404, error405)
import Nodetrout.Internal.Error (select) as Error
import Nodetrout.Internal.Request (Request)
import Nodetrout.Internal.Request (headerValue, method, path, queryParamValue, queryParamValues, removePath, stringBody, unconsPath) as Request
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get) as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Trout (Method) as Trout
import Type.Trout (type (:<|>), type (:>), type (:=), Capture, CaptureAll, Header, Lit, QueryParam, QueryParams, ReqBody, Resource)
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeParse, class MimeRender, allMimeRender, getMediaType, mimeParse, mimeRender)
import Type.Trout.Header (class FromHeader, fromHeader)
import Type.Trout.PathPiece (class FromPathPiece, fromPathPiece)

-- | The specificity of a route, useful for prioritizing error responses
type Depth = Int

-- | Routes a request using the specified layout and handlers.
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
        throwError error404 { priority = depth }

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
            throwError error404 { priority = depth }
      _ ->
        throwError error404 { priority = depth }

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
          throwError error404 { priority = depth }

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
              throwError error400 { priority = depth, details = Just ("Invalid value for query parameter " <> label) }
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
          throwError error400 { priority = depth, details = Just ("Invalid value for query parameter " <> label) }

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
          throwError error400 { priority = depth, details = Just ("Header " <> name <> ": " <> error) }

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
            throwError error400 { priority = depth, details = Just ("Request body not in expected format: " <> e) }
      Nothing ->
        throwError error400 { priority = depth, details = Just "A request body is required, but none was provided." }

instance routerMethod ::
  ( Monad m
  , IsSymbol method
  , DeferredAllMimeRender body contentTypes rendered
  , Row.Cons method (ExceptT HTTPError m body) handlers' handlers
  ) => Router (Trout.Method method body contentTypes) (Record handlers) m (Tuple MediaType rendered) where
  route layout handlers request depth = do
    let method = SProxy :: SProxy method
    when (not $ null $ Request.path request) $ throwError error404 { priority = depth }
    when (Request.method request /= Method.fromString (reflectSymbol method)) $ throwError error405 { priority = depth }
    let runBody = Record.get method handlers
    content <- Content.negotiate request (deferredMimeRenderers (Proxy :: Proxy contentTypes)) runBody
    pure content

instance routerResource ::
  ( Monad m
  , Router layout handlers m result
  ) => Router (Resource layout) handlers m result where
  route _ = route (Proxy :: Proxy layout)


-- | Workaround for AllMimeRender's design somewhat defeating its own point
-- | There's no way to determine at runtime if a given `a` *can* be rendered as `ct` with some
-- | given `MediaType`. You need to supply an `a`, and then iterate through the returned list,
-- | in other words, `AllMimeRender` forces you to evaluate your endpoint before feeding
-- | the result into `allMimeRender`. Of course, there's no sense in evaluating an endpoint if
-- | the client can't consume it. In fact, it's very dangerous, because then you'd potentially perform
-- | a stateful action but then tell the client we couldn't serve their request with the implication
-- | that it was refused.
-- |
-- | This is also convenient because now Nodetrout doesn't need a bunch of repetitive
-- | AllMimeRender instances to make routers for non-alt types.
-- | i.e., you dont have to make `instance HasMediaType ct` and `MimeRender a ct b`
-- | only to then make `instance AllMimeRender a ct b` which just returns a tuple of the two...
class DeferredAllMimeRender a cts b | a -> b, cts -> b where
  deferredMimeRenderers :: Proxy cts -> NonEmptyList (Tuple MediaType (a -> b))

instance deferredAllMimeRenderExtAlt ::
  ( HasMediaType ct1
  , MimeRender a ct1 b
  , DeferredAllMimeRender a ct2 b
  ) => DeferredAllMimeRender a (ct1 :<|> ct2) b where
    deferredMimeRenderers _ = pure (Tuple (getMediaType p) (mimeRender p)) <> (deferredMimeRenderers p')
      where p = Proxy :: Proxy ct1
            p' = Proxy :: Proxy ct2
else instance deferredAllMimeRenderExtSingle ::
  ( HasMediaType ct
  , MimeRender a ct b
  ) => DeferredAllMimeRender a ct b where
    deferredMimeRenderers _ = pure (Tuple (getMediaType p) (mimeRender p))
      where p = Proxy :: Proxy ct