module  Types where

import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map


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

-- | Type to handle Client satistics
type ClientStats = Map.Map Int (Int, Int)
