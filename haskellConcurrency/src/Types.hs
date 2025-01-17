module  Types (
    Request(..),
    Response(..),
    RequestQueue
) where

import Data.Time.Clock (UTCTime)


-- | Type to send request from a client 
data Request = Request {
    reqestID :: Int,
    requestContent :: String,
    requestTime :: UTCTime
    
} deriving (Show)


-- | Type to send response from a server
data Response = Response {
    respId :: Int,
    respContent :: String,
    respTime :: UTCTime
} deriving (Show)

-- | Type handle the request queue 
type RequestQueue = [Request]
