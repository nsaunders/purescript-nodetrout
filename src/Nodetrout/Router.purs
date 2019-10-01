module Nodetrout.Router where
  
import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, filter, null, uncons)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.HTTP.Method (fromString) as Method
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref (modify_, new, read) as Ref
import Network.HTTP (status400, status404, status405)
import Node.Buffer (concat, toString) as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onData, onEnd) as Stream
import Nodetrout.Content (negotiate) as Content
import Nodetrout.Context (Context)
import Nodetrout.Error (select) as Error
import Nodetrout.Error (HTTPError(..))
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get) as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>), type (:>), type (:=), Capture, CaptureAll, Lit, QueryParam, QueryParams, ReqBody, Resource)
import Type.Trout (Method) as Trout
import Type.Trout.ContentType (class AllMimeRender, class MimeParse, allMimeRender, mimeParse)
import Type.Trout.PathPiece (class FromPathPiece, fromPathPiece)

class Router layout handlers result | layout -> handlers, layout -> result where
  route :: Proxy layout -> handlers -> Context -> result

instance routerAltNamed ::
  ( Monad m
  , Router layout handler (ExceptT HTTPError m result)
  , Router otherLayout (Record otherHandlers) (ExceptT HTTPError m result)
  , IsSymbol name
  , Row.Cons name handler otherHandlers handlers
  , Row.Lacks name otherHandlers
  ) => Router (name := layout :<|> otherLayout) (Record handlers) (ExceptT HTTPError m result) where
  route _ handlers context = do
    eitherResult1 <- lift $ runExceptT $ route (Proxy :: Proxy layout) (Record.get name handlers) context
    case eitherResult1 of
      Right result ->
        pure result
      Left error1 -> do
        eitherResult2 <- lift $ runExceptT $ route (Proxy :: Proxy otherLayout) (Record.delete name handlers) context
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
  route _ handlers context =
    case uncons context.path of
      Just { head, tail } | head == reflectSymbol (SProxy :: SProxy segment) ->
        route (Proxy :: Proxy layout) handlers (context { path = tail })
      _ ->
        throwError $ HTTPError { status: status404, details: Nothing }

instance routerCapture ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , FromPathPiece value
  ) => Router (Capture label value :> layout) (value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers context =
    case uncons context.path of
      Nothing ->
        throwError $ HTTPError { status: status404, details: Nothing }
      Just { head, tail } ->
        case fromPathPiece head of
          Left error ->
            throwError $ HTTPError { status: status400, details: Just error }
          Right value ->
            route (Proxy :: Proxy layout) (handlers value) context { path = tail }

instance routerCaptureAll ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , FromPathPiece value
  ) => Router (CaptureAll label value :> layout) (Array value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers context =
    case traverse fromPathPiece context.path of
      Left _ ->
        throwError $ HTTPError { status: status404, details: Nothing }
      Right value ->
        route (Proxy :: Proxy layout) (handlers value) context { path = [] }

instance routerQueryParam ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , IsSymbol label
  , FromPathPiece value
  ) => Router (QueryParam label value :> layout) (Maybe value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers context =
    let
      label = reflectSymbol (SProxy :: SProxy label)
    in
      case (join $ snd <$> find ((_ == label) <<< fst) context.query) of
        Just param ->
          case fromPathPiece param of
            Right value ->
              route (Proxy :: Proxy layout) (handlers $ Just value) context
            Left _ ->
              throwError $ HTTPError { status: status400, details: Just $ "Invalid value for query parameter " <> label }
        Nothing ->
          route (Proxy :: Proxy layout) (handlers Nothing) context

instance routerQueryParams ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m next)
  , IsSymbol label
  , FromPathPiece value
  ) => Router (QueryParams label value :> layout) (Array value -> handlers) (ExceptT HTTPError m next) where
  route _ handlers context =
    let
      label = reflectSymbol (SProxy :: SProxy label)
    in
      case (traverse fromPathPiece $ catMaybes $ map snd $ filter ((_ == label) <<< fst) context.query) of
        Right values ->
          route (Proxy :: Proxy layout) (handlers values) context
        Left _ ->
          throwError $ HTTPError { status: status400, details: Just $ "Invalid value for query parameter " <> label }

instance routerReqBody ::
  ( Monad m
  , MonadAff m
  , Router layout handlers (ExceptT HTTPError m next)
  , MimeParse String contentType parsed
  ) => Router (ReqBody parsed contentType :> layout) (parsed -> handlers) (ExceptT HTTPError m next) where
  route _ handlers context = do
    maybeBody <- liftAff $ makeAff \done -> do
                   chunks <- Ref.new []
                   Stream.onData context.body \chunk -> Ref.modify_ (cons chunk) chunks
                   Stream.onEnd context.body $ Ref.read chunks >>=
                     case _ of
                       [] ->
                         done $ Right Nothing
                       chx ->
                         Buffer.concat chx >>= Buffer.toString UTF8 >>= Just >>> Right >>> done
                   pure nonCanceler
    case maybeBody of
      Just body ->
        case mimeParse (Proxy :: Proxy contentType) body of
          Right parsed ->
            route (Proxy :: Proxy layout) (handlers parsed) context
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
  route layout handlers context = do
    let method = SProxy :: SProxy method
    when (not $ null context.path)
      $ throwError $ HTTPError { status: status404, details: Nothing }
    when (context.method /= Method.fromString (reflectSymbol method))
      $ throwError $ HTTPError { status: status405, details: Nothing }
    body <- Record.get method handlers
    content <- Content.negotiate context $ allMimeRender (Proxy :: Proxy contentTypes) body
    pure content

instance routerResource ::
  ( Monad m
  , Router layout handlers (ExceptT HTTPError m result)
  ) => Router (Resource layout) handlers (ExceptT HTTPError m result) where
  route _ = route (Proxy :: Proxy layout)
