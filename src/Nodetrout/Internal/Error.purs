module Nodetrout.Internal.Error where
  
import Prelude
import Data.Lens (Lens', (^.), lens)
import Data.Maybe (Maybe(..))

newtype HTTPError = HTTPError
  { statusCode :: Int
  , overview :: String
  , details :: Maybe String
  , priority :: Int
  }

instance showHTTPError :: Show HTTPError where
  show (HTTPError error) = "HTTPError (" <> show error <> ")"

_errorStatusCode :: Lens' HTTPError Int
_errorStatusCode = lens
  (\(HTTPError { statusCode }) -> statusCode)
  (\(HTTPError e) statusCode -> HTTPError e { statusCode = statusCode })

_errorOverview :: Lens' HTTPError String
_errorOverview = lens
  (\(HTTPError { overview }) -> overview)
  (\(HTTPError e) overview -> HTTPError e { overview = overview })

_errorDetails :: Lens' HTTPError (Maybe String)
_errorDetails = lens
  (\(HTTPError { details }) -> details)
  (\(HTTPError e) details -> HTTPError e { details = details })

_errorPriority :: Lens' HTTPError Int
_errorPriority = lens
  (\(HTTPError { priority }) -> priority)
  (\(HTTPError e) priority -> HTTPError e { priority = priority })

select :: HTTPError -> HTTPError -> HTTPError
select error1 error2
  | error1 ^. _errorPriority > error2 ^. _errorPriority = error1
  | error1 ^. _errorPriority < error2 ^. _errorPriority = error2
  | statusCodePriority error1 < statusCodePriority error2 = error2
  | otherwise = error1

statusCodePriority :: HTTPError -> Int
statusCodePriority error = case error ^. _errorStatusCode of
  404 -> 0
  405 -> 1
  401 -> 2
  415 -> 3
  406 -> 4
  400 -> 6
  _ -> 5

defaultError :: Int -> String -> HTTPError
defaultError statusCode overview = HTTPError { statusCode, overview, details: Nothing, priority: 1000000 }

error300 :: HTTPError
error300 = defaultError 300 "Multiple Choices"

error301 :: HTTPError
error301 = defaultError 301 "Moved Permanently"

error302 :: HTTPError
error302 = defaultError 302 "Found"

error303 :: HTTPError
error303 = defaultError 303 "See Other"

error304 :: HTTPError
error304 = defaultError 304 "Not Modified"

error305 :: HTTPError
error305 = defaultError 305 "Use Proxy"

error307 :: HTTPError
error307 = defaultError 307 "Temporary Redirect"

error400 :: HTTPError
error400 = defaultError 400 "Bad Request"

error401 :: HTTPError
error401 = defaultError 401 "Unauthorized"

error402 :: HTTPError
error402 = defaultError 402 "Payment Required"

error403 :: HTTPError
error403 = defaultError 403 "Forbidden"

error404 :: HTTPError
error404 = defaultError 404 "Not Found"

error405 :: HTTPError
error405 = defaultError 405 "Method Not Allowed"

error406 :: HTTPError
error406 = defaultError 406 "Not Acceptable"

error407 :: HTTPError
error407 = defaultError 407 "Proxy Authentication Required"

error409 :: HTTPError
error409 = defaultError 409 "Conflict"

error410 :: HTTPError
error410 = defaultError 410 "Gone"

error411 :: HTTPError
error411 = defaultError 411 "Length Required"

error412 :: HTTPError
error412 = defaultError 412 "Precondition Failed"

error413 :: HTTPError
error413 = defaultError 413 "Request Entity Too Large"

error414 :: HTTPError
error414 = defaultError 414 "Request-URI Too Large"

error415 :: HTTPError
error415 = defaultError 415 "Unsupported Media Type"

error416 :: HTTPError
error416 = defaultError 416 "Request range not satisfiable"

error417 :: HTTPError
error417 = defaultError 417 "Expectation Failed"

error418 :: HTTPError
error418 = defaultError 418 "I'm a teapot"

error422 :: HTTPError
error422 = defaultError 422 "Unprocessable Entity"

error500 :: HTTPError
error500 = defaultError 500 "Internal Server Error"

error501 :: HTTPError
error501 = defaultError 501 "Not Implemented"

error502 :: HTTPError
error502 = defaultError 502 "Bad Gateway"

error503 :: HTTPError
error503 = defaultError 503 "Service Unavailable"

error504 :: HTTPError
error504 = defaultError 504 "Gateway Time-out"

error505 :: HTTPError
error505 = defaultError 505 "HTTP Version not supported"
