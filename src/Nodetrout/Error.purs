module Nodetrout.Error where
  
import Prelude
import Data.Maybe (Maybe)
import Network.HTTP (StatusCode(MethodNotAllowed, NotFound))

newtype HTTPError = HTTPError
  { status :: StatusCode
  , details :: Maybe String
  }

isNotFound :: HTTPError -> Boolean
isNotFound (HTTPError { status }) = case status of
  NotFound -> true
  _ -> false

isMethodNotAllowed :: HTTPError -> Boolean
isMethodNotAllowed (HTTPError { status }) = case status of
  MethodNotAllowed -> true
  _ -> false
  
select :: HTTPError -> HTTPError -> HTTPError
select error1 error2
  | (isMethodNotAllowed error1 || isNotFound error1) && isNotFound error2 = error1
  | not (isNotFound error1) && (isMethodNotAllowed error2 || isNotFound error2) = error1
  | otherwise = error2
