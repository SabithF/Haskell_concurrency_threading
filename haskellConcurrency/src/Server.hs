module Server where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Time.Clock (getCurrentTime)
import Types

-- | 
initServer :: MVar RequestQueue -> Int -> MVar () -> IO ()
initServer queue maxReq serverDone = loop 0
    where
        loop :: Int -> IO ()
        loop reqCount 
            | reqCount >= maxReq = do
                putStrLn "Server has processed all the requests"
                putMVar serverDone () -- Signaling the server is done
            | otherwise = do
                reqQueueList <-  takeMVar queue
                case reqQueueList of
                    [] -> do
                        putMVar queue reqQueueList 
                        threadDelay 100000
                        loop reqCount
                    (req:rest) -> do
                        putMVar queue rest

                        currentTime <- getCurrentTime
                        let response = Response (reqestID req) ("Response to the request " ++ requestContent req) currentTime

                        -- | Writing the logger file reqiests.log
                        appendFile "requests.log" (show(req, response) ++ "\n")
                        putStrLn $ "Request from the Client ID: " ++ show (reqestID req) ++ " has been processed successfully"
                        loop (reqCount + 1)


