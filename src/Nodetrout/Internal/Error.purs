module Nodetrout.Internal.Error where
  
import Prelude
import Data.Maybe (Maybe(..))

type HTTPError =
  { statusCode :: Int
  , overview :: String
  , details :: Maybe String
  , priority :: Int
  }

select :: HTTPError -> HTTPError -> HTTPError
select error1 error2
  | error1.priority > error2.priority = error1
  | error1.priority < error2.priority = error2
  | statusCodePriority error1 < statusCodePriority error2 = error2
  | otherwise = error1

statusCodePriority :: HTTPError -> Int
statusCodePriority { statusCode } = case statusCode of
  404 -> 0
  405 -> 1
  401 -> 2
  415 -> 3
  406 -> 4
  400 -> 6
  _ -> 5

mkError :: Int -> String -> HTTPError
mkError statusCode overview = { statusCode, overview, details: Nothing, priority: 1000000 }

error300 :: HTTPError
error300 = mkError 300 "Multiple Choices"

error301 :: HTTPError
error301 = mkError 301 "Moved Permanently"

error302 :: HTTPError
error302 = mkError 302 "Found"

error303 :: HTTPError
error303 = mkError 303 "See Other"

error304 :: HTTPError
error304 = mkError 304 "Not Modified"

error305 :: HTTPError
error305 = mkError 305 "Use Proxy"

error307 :: HTTPError
error307 = mkError 307 "Temporary Redirect"

error400 :: HTTPError
error400 = mkError 400 "Bad Request"

error401 :: HTTPError
error401 = mkError 401 "Unauthorized"

error402 :: HTTPError
error402 = mkError 402 "Payment Required"

error403 :: HTTPError
error403 = mkError 403 "Forbidden"

error404 :: HTTPError
error404 = mkError 404 "Not Found"

error405 :: HTTPError
error405 = mkError 405 "Method Not Allowed"

error406 :: HTTPError
error406 = mkError 406 "Not Acceptable"

error407 :: HTTPError
error407 = mkError 407 "Proxy Authentication Required"

error409 :: HTTPError
error409 = mkError 409 "Conflict"

error410 :: HTTPError
error410 = mkError 410 "Gone"

error411 :: HTTPError
error411 = mkError 411 "Length Required"

error412 :: HTTPError
error412 = mkError 412 "Precondition Failed"

error413 :: HTTPError
error413 = mkError 413 "Request Entity Too Large"

error414 :: HTTPError
error414 = mkError 414 "Request-URI Too Large"

error415 :: HTTPError
error415 = mkError 415 "Unsupported Media Type"

error416 :: HTTPError
error416 = mkError 416 "Request range not satisfiable"

error417 :: HTTPError
error417 = mkError 417 "Expectation Failed"

error418 :: HTTPError
error418 = mkError 418 "I'm a teapot"

error422 :: HTTPError
error422 = mkError 422 "Unprocessable Entity"

error500 :: HTTPError
error500 = mkError 500 "Internal Server Error"

error501 :: HTTPError
error501 = mkError 501 "Not Implemented"

error502 :: HTTPError
error502 = mkError 502 "Bad Gateway"

error503 :: HTTPError
error503 = mkError 503 "Service Unavailable"

error504 :: HTTPError
error504 = mkError 504 "Gateway Time-out"

error505 :: HTTPError
error505 = mkError 505 "HTTP Version not supported"
