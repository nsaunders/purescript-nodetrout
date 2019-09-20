module Nodetrout.Router where
  
import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Data.Array (null, uncons)
import Data.HTTP.Method (fromString) as Method
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), reflectSymbol)
import Network.HTTP (status404, status405)
import Nodetrout.Context (Context)
import Nodetrout.Error (HTTPError(..))
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:>), type (:=), Lit, Resource)
import Type.Trout (Method) as Trout
import Type.Trout.ContentType (class AllMimeRender)

class Router layout handlers m result | layout -> handlers, layout -> result where
  route :: Proxy layout -> handlers -> Context -> ExceptT HTTPError m result

instance routerNamed ::
  ( Monad m
  , Router layout handler m result
  , IsSymbol name
  , Row.Cons name handler () handlers
  ) => Router (name := layout) (Record handlers) m result where
  route _ handlers = route (Proxy :: Proxy layout) (Record.get (SProxy :: SProxy name) handlers)

instance routerLit ::
  ( Monad m
  , Router layout handlers m result
  , IsSymbol segment
  ) => Router (Lit segment :> layout) handlers m result where
    route _ handlers context =
      case uncons context.path of
        Just { head, tail } | head == reflectSymbol (SProxy :: SProxy segment) ->
          route (Proxy :: Proxy layout) handlers (context { path = tail })
        _ ->
          throwError $ HTTPError { status: status404, details: Nothing }

instance routerMethod ::
  ( Monad m
  , IsSymbol method
  , AllMimeRender body contentTypes rendered
  , Row.Cons method (ExceptT HTTPError m body) handlers' handlers
  ) => Router (Trout.Method method body contentTypes) (Record handlers) m Unit where
  route layout handlers context = do
    routeEnd (SProxy :: SProxy method) context
    body <- Record.get (SProxy :: SProxy method) handlers
    pure unit -- todo content negotiation and render body

instance routerResource ::
  ( Monad m
  , Router layout handlers m result
  ) => Router (Resource layout) handlers m result where
  route _ = route (Proxy :: Proxy layout)

routeEnd
  :: forall m method
   . Monad m
  => IsSymbol method
  => SProxy method
  -> Context
  -> ExceptT HTTPError m Unit
routeEnd methodProxy context = do
  when (not $ null context.path)
    $ throwError $ HTTPError { status: status404, details: Nothing }
  when (context.method /= Method.fromString (reflectSymbol methodProxy))
    $ throwError $ HTTPError { status: status405, details: Nothing }
  pure unit
