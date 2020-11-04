-- | This module contains the routing logic.
module Nodetrout.Internal.Router
  ( class AllMimeParse
  , class DeferredAllMimeRender
  , class FromByteString
  , class Router
  , allMimeParse
  , deferredMimeRenderers
  , fromByteString
  , route
  ) where
  
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (null)
import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Either (Either(..), note)
import Data.HTTP.Method (fromString) as Method
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Nodetrout.Internal.Content (class ResponseWritable, ResponseWriter, mkResponseWriter)
import Nodetrout.Internal.Content (negotiate) as Content
import Nodetrout.Internal.Error (HTTPError, error400, error404, error405)
import Nodetrout.Internal.Error (select) as Error
import Nodetrout.Internal.Request (Request, headerValue)
import Nodetrout.Internal.Request (bytestringBody, headerValue, method, path, queryParamValue, queryParamValues, removePath, toUnparameterizedMediaType, unconsPath) as Request
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get) as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Trout (Method) as Trout
import Type.Trout (type (:<|>), type (:>), type (:=), Capture, CaptureAll, Header, Lit, QueryParam, QueryParams, ReqBody, Resource)
import Type.Trout.ContentType (class HasMediaType, class MimeParse, class MimeRender, getMediaType, mimeParse, mimeRender)
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
  , AllMimeParse contentType parsed
  ) => Router (ReqBody parsed contentType :> layout) (parsed -> handlers) m result where
  route _ handlers request depth =
    liftAff (Request.bytestringBody request) >>= case _ of
      Just body ->
        let bodyContentType = Request.toUnparameterizedMediaType =<< headerValue "content-type" request
         in case allMimeParse (Proxy :: Proxy contentType) bodyContentType body of
              Right parsed ->
                route (Proxy :: Proxy layout) (handlers parsed) request $ depth + 1
              Left e ->
                throwError error400 { priority = depth, details = Just ("Request body not in expected format: " <> e) }
      Nothing ->
        throwError error400 { priority = depth, details = Just "A request body is required, but none was provided." }

instance routerMethod ::
  ( Monad m
  , IsSymbol method
  , DeferredAllMimeRender body contentTypes
  , Row.Cons method (ExceptT HTTPError m body) handlers' handlers
  ) => Router (Trout.Method method body contentTypes) (Record handlers) m (Tuple MediaType ResponseWriter) where
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
class DeferredAllMimeRender a cts where
  deferredMimeRenderers ::Proxy cts -> NonEmptyList (Tuple MediaType (a -> ResponseWriter))

instance deferredAllMimeRenderExtAlt ::
  ( HasMediaType ct1
  , MimeRender a ct1 b1
  , ResponseWritable b1
  , DeferredAllMimeRender a ct2
  ) => DeferredAllMimeRender a (ct1 :<|> ct2) where
    deferredMimeRenderers _ = pure (Tuple (getMediaType p) (mkResponseWriter <<< mimeRender p)) <> (wrapResponseWriter $ deferredMimeRenderers p')
      where p = Proxy :: Proxy ct1
            p' = Proxy :: Proxy ct2
            pb1 = Proxy :: Proxy b1
            wrapResponseWriter = map (map (map mkResponseWriter))
else instance deferredAllMimeRenderExtSingle ::
  ( HasMediaType ct
  , MimeRender a ct b2
  , ResponseWritable b2
  ) => DeferredAllMimeRender a ct where
    deferredMimeRenderers _ = pure (Tuple (getMediaType p) (mkResponseWriter <<< mimeRender p))
      where p = Proxy :: Proxy ct

-- | This is kinda like deferredAllMimeRender above, but for `ReqBodies`
class AllMimeParse cts a | cts -> a where
  allMimeParse :: Proxy cts -> Maybe MediaType -> ByteString -> Either String a

instance allMimeParseAlt ::
  ( AllMimeParse ct1 a
  , AllMimeParse ct2 a
  ) => AllMimeParse (ct1 :<|> ct2) a where
    allMimeParse _ mt body = allMimeParse p1 mt body <|> allMimeParse p2 mt body
      where p1 = Proxy :: Proxy ct1
            p2 = Proxy :: Proxy ct2
else instance allMimeParseSingle ::
  ( HasMediaType ct
  , FromByteString b
  , MimeParse b ct a
  ) => AllMimeParse ct a where
    allMimeParse _ mt =
      let go = mimeParse p <<< fromByteString
          p = Proxy :: Proxy ct
       in case mt of
            Nothing -> go
            Just mt' -> if mt' == getMediaType p
                        then go
                        else \_ -> Left "Content-Type is not supported by this server"

-- | This is needed because PureScript's instance resolution doesn't behave as expected. 
-- | The above AllMimeParse instances would ideally be:
-- |   instance ampAlt :: (AMP ct1 a, AMP ct2 a) => AMP (ct1 :<|> ct2) a
-- |   else instance ampBS :: (HasMediaType ct, MimeParse BS ct a) => AMP ct a
-- |   else instance ampStr :: (HasMediaType ct, MimeParse String ct a) => AMP ct a
-- | However, `purs` will instead stick to expecting everything to be whatever is the first `else instance`, 
-- | probably to avoid undecidability. I think this is also why deferredAllMimeRenderExtAlt needs to be
-- | explicit in its first `Alt` argument. However, there's a possibility for a need of n^2 instances in the case of
-- | AllMimeParse, so this lets us avoid that
class FromByteString a where
  fromByteString :: ByteString -> a

instance fromByteStringBS :: FromByteString ByteString where
  fromByteString x = x
instance isBSIsoString :: FromByteString String where
  fromByteString = ByteString.fromUTF8