module Nodetrout.Error where
  
import Prelude
import Data.Foldable (elem)
import Data.Lens (Lens', (^.), lens)
import Data.Maybe (Maybe(..))

newtype HTTPError = HTTPError
  { statusCode :: Int
  , overview :: String
  , details :: Maybe String
  }

_errorStatusCode :: Lens' HTTPError Int
_errorStatusCode = lens
  (\(HTTPError { statusCode }) -> statusCode)
  (\(HTTPError e) statusCode -> HTTPError $ e { statusCode = statusCode })

_errorOverview :: Lens' HTTPError String
_errorOverview = lens
  (\(HTTPError { overview }) -> overview)
  (\(HTTPError e) overview -> HTTPError $ e { overview = overview })

_errorDetails :: Lens' HTTPError (Maybe String)
_errorDetails = lens
  (\(HTTPError { details }) -> details)
  (\(HTTPError e) details -> HTTPError $ e { details = details })
 
select :: HTTPError -> HTTPError -> HTTPError
select error1 error2
  | (error1 ^. _errorStatusCode) `elem` [404, 405] && error2 ^. _errorStatusCode == 404 = error1
  | error1 ^. _errorStatusCode /= 404 && (error2 ^. _errorStatusCode) `elem` [404, 405] = error1
  | otherwise = error2

error300 :: HTTPError
error300 = HTTPError { statusCode: 300, overview: "Multiple Choices", details: Nothing }

error301 :: HTTPError
error301 = HTTPError { statusCode: 301, overview: "Moved Permanently", details: Nothing }

error302 :: HTTPError
error302 = HTTPError { statusCode: 302, overview: "Found", details: Nothing }

error303 :: HTTPError
error303 = HTTPError { statusCode: 303, overview: "See Other", details: Nothing }

error304 :: HTTPError
error304 = HTTPError { statusCode: 304, overview: "Not Modified", details: Nothing }

error305 :: HTTPError
error305 = HTTPError { statusCode: 305, overview: "Use Proxy", details: Nothing }

error307 :: HTTPError
error307 = HTTPError { statusCode: 307, overview: "Temporary Redirect", details: Nothing }

error400 :: HTTPError
error400 = HTTPError { statusCode: 400, overview: "Bad Request", details: Nothing }

error401 :: HTTPError
error401 = HTTPError { statusCode: 401, overview: "Unauthorized", details: Nothing }

error402 :: HTTPError
error402 = HTTPError { statusCode: 402, overview: "Payment Required", details: Nothing }

error403 :: HTTPError
error403 = HTTPError { statusCode: 403, overview: "Forbidden", details: Nothing }

error404 :: HTTPError
error404 = HTTPError { statusCode: 404, overview: "Not Found", details: Nothing }

error405 :: HTTPError
error405 = HTTPError { statusCode: 405, overview: "Method Not Allowed", details: Nothing }

error406 :: HTTPError
error406 = HTTPError { statusCode: 406, overview: "Not Acceptable", details: Nothing }

error407 :: HTTPError
error407 = HTTPError { statusCode: 407, overview: "Proxy Authentication Required", details: Nothing }

error409 :: HTTPError
error409 = HTTPError { statusCode: 409, overview: "Conflict", details: Nothing }

error410 :: HTTPError
error410 = HTTPError { statusCode: 410, overview: "Gone", details: Nothing }

error411 :: HTTPError
error411 = HTTPError { statusCode: 411, overview: "Length Required", details: Nothing }

error412 :: HTTPError
error412 = HTTPError { statusCode: 412, overview: "Precondition Failed", details: Nothing }

error413 :: HTTPError
error413 = HTTPError { statusCode: 413, overview: "Request Entity Too Large", details: Nothing }

error414 :: HTTPError
error414 = HTTPError { statusCode: 414, overview: "Request-URI Too Large", details: Nothing }

error415 :: HTTPError
error415 = HTTPError { statusCode: 415, overview: "Unsupported Media Type", details: Nothing }

error416 :: HTTPError
error416 = HTTPError { statusCode: 416, overview: "Request range not satisfiable", details: Nothing }

error417 :: HTTPError
error417 = HTTPError { statusCode: 417, overview: "Expectation Failed", details: Nothing }

error418 :: HTTPError
error418 = HTTPError { statusCode: 418, overview: "I'm a teapot", details: Nothing }

error422 :: HTTPError
error422 = HTTPError { statusCode: 422, overview: "Unprocessable Entity", details: Nothing }

error500 :: HTTPError
error500 = HTTPError { statusCode: 500, overview: "Internal Server Error", details: Nothing }

error501 :: HTTPError
error501 = HTTPError { statusCode: 501, overview: "Not Implemented", details: Nothing }

error502 :: HTTPError
error502 = HTTPError { statusCode: 502, overview: "Bad Gateway", details: Nothing }

error503 :: HTTPError
error503 = HTTPError { statusCode: 503, overview: "Service Unavailable", details: Nothing }

error504 :: HTTPError
error504 = HTTPError { statusCode: 504, overview: "Gateway Time-out", details: Nothing }

error505 :: HTTPError
error505 = HTTPError { statusCode: 505, overview: "HTTP Version not supported", details: Nothing }
