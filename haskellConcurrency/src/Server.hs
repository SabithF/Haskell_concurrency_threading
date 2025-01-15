module Server where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Time.Clock (getCurrentTime)
import Types
import Control.Monad (void)

-- | Server function to process requests
initServer :: MVar RequestQueue -> Int -> MVar Int -> MVar Bool -> MVar () -> IO ()
initServer queue maxReq processedCounter serverActive serverDone = processRequests 0
  where
    processRequests :: Int -> IO ()
    processRequests count
        | count >= maxReq = do
            putStrLn "All the requests have been processed successfully."
            swapMVar serverActive False -- Signal clients to stop adding requests
            putMVar serverDone () -- Signal the server is done
        | otherwise = do
            reqQueueList <- takeMVar queue
            case reqQueueList of
                [] -> do
                    putMVar queue reqQueueList
                    threadDelay 100000
                    processRequests count
                (req:rest) -> do
                    if count + 1 == maxReq
                        then void $ swapMVar serverActive False -- Signal stop before processing the last request
                        else return ()

                    putMVar queue rest
                    currentTime <- getCurrentTime
                    let response = Response (reqestID req) ("Response to the request " ++ requestContent req) currentTime

                    appendFile "requests.log" (show (req, response) ++ "\n")
                    modifyMVar_ processedCounter (\c -> return (c + 1)) -- Increment the response counter
                    putStrLn $ "Processed request from Client ID: " ++ show (reqestID req)
                    processRequests (count + 1)
