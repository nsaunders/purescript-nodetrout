-- | This module contains error-related types and logic.
module Nodetrout.Internal.Error
  ( HTTPError
  , error300
  , error301
  , error302
  , error303
  , error304
  , error305
  , error307
  , error400
  , error401
  , error402
  , error403
  , error404
  , error405
  , error406
  , error407
  , error409
  , error410
  , error411
  , error412
  , error413
  , error414
  , error415
  , error416
  , error417
  , error418
  , error422
  , error500
  , error501
  , error502
  , error503
  , error504
  , error505
  , select
  ) where
  
import Prelude
import Data.Maybe (Maybe(..))

-- | The type of an HTTP error, part of the handler type `ExceptT HTTPError m a`
type HTTPError =
  { statusCode :: Int
  , overview :: String
  , details :: Maybe String
  , priority :: Int
  }

-- | Given two `HTTPError` values, determines the best response based on priority
-- | or, when equal by priority, status code. The router assigns a lower priority
-- | based on route specificity/depth so that the error presented to the client
-- | is the one that is easier to resolve.
select :: HTTPError -> HTTPError -> HTTPError
select error1 error2
  | error1.priority > error2.priority = error1
  | error1.priority < error2.priority = error2
  | statusCodePriority error1 < statusCodePriority error2 = error2
  | otherwise = error1

-- | Prioritizes status codes.
statusCodePriority :: HTTPError -> Int
statusCodePriority { statusCode } = case statusCode of
  404 -> 0
  405 -> 1
  401 -> 2
  415 -> 3
  406 -> 4
  400 -> 6
  _ -> 5

-- | Initializes an `HTTPError` with the specified status code and overview.
mkError :: Int -> String -> HTTPError
mkError statusCode overview = { statusCode, overview, details: Nothing, priority: 1000000 }

-- | A `Multiple Choices` error
error300 :: HTTPError
error300 = mkError 300 "Multiple Choices"

-- | A `Moved Permanently` error
error301 :: HTTPError
error301 = mkError 301 "Moved Permanently"

-- | A `Found` error
error302 :: HTTPError
error302 = mkError 302 "Found"

-- | A `See Other` error
error303 :: HTTPError
error303 = mkError 303 "See Other"

-- | A `Not Modified` error
error304 :: HTTPError
error304 = mkError 304 "Not Modified"

-- | A `Use Proxy` error
error305 :: HTTPError
error305 = mkError 305 "Use Proxy"

-- | A `Temporary Redirect` error
error307 :: HTTPError
error307 = mkError 307 "Temporary Redirect"

-- | A `Bad Request` error
error400 :: HTTPError
error400 = mkError 400 "Bad Request"

-- | A `Unauthorized` error
error401 :: HTTPError
error401 = mkError 401 "Unauthorized"

-- | A `Payment Required` error
error402 :: HTTPError
error402 = mkError 402 "Payment Required"

-- | A `Forbidden` error
error403 :: HTTPError
error403 = mkError 403 "Forbidden"

-- | A `Not Found` error
error404 :: HTTPError
error404 = mkError 404 "Not Found"

-- | A `Method Not Allowed` error
error405 :: HTTPError
error405 = mkError 405 "Method Not Allowed"

-- | A `Not Acceptable` error
error406 :: HTTPError
error406 = mkError 406 "Not Acceptable"

-- | A `Proxy Authentication Required` error
error407 :: HTTPError
error407 = mkError 407 "Proxy Authentication Required"

-- | A `Conflict` error
error409 :: HTTPError
error409 = mkError 409 "Conflict"

-- | A `Gone` error
error410 :: HTTPError
error410 = mkError 410 "Gone"

-- | A `Length Required` error
error411 :: HTTPError
error411 = mkError 411 "Length Required"

-- | A `Precondition Failed` error
error412 :: HTTPError
error412 = mkError 412 "Precondition Failed"

-- | A `Request Entity Too Large` error
error413 :: HTTPError
error413 = mkError 413 "Request Entity Too Large"

-- | A `Request-URI Too Large` error
error414 :: HTTPError
error414 = mkError 414 "Request-URI Too Large"

-- | A `Unsupported Media Type` error
error415 :: HTTPError
error415 = mkError 415 "Unsupported Media Type"

-- | A `Request range not satisfiable` error
error416 :: HTTPError
error416 = mkError 416 "Request range not satisfiable"

-- | A `Expectation Failed` error
error417 :: HTTPError
error417 = mkError 417 "Expectation Failed"

-- | A `I'm a teapot` error
error418 :: HTTPError
error418 = mkError 418 "I'm a teapot"

-- | A `Unprocessable Entity` error
error422 :: HTTPError
error422 = mkError 422 "Unprocessable Entity"

-- | A `Internal Server Error` error
error500 :: HTTPError
error500 = mkError 500 "Internal Server Error"

-- | A `Not Implemented` error
error501 :: HTTPError
error501 = mkError 501 "Not Implemented"

-- | A `Bad Gateway` error
error502 :: HTTPError
error502 = mkError 502 "Bad Gateway"

-- | A `Service Unavailable` error
error503 :: HTTPError
error503 = mkError 503 "Service Unavailable"

-- | A `Gateway Time-out` error
error504 :: HTTPError
error504 = mkError 504 "Gateway Time-out"

-- | A `HTTP Version not supported` error
error505 :: HTTPError
error505 = mkError 505 "HTTP Version not supported"
