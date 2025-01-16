{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Control.Concurrent
import Data.Time.Clock (getCurrentTime)
import Types
import System.IO.Error (tryIOError)
import Control.Exception (IOException)

-- | Function to initialize the server 
initServer :: Chan Request -> Int -> MVar Int -> MVar Int -> MVar Bool -> MVar () -> IO ()
initServer requestQueue maxReq requestCounter responseCounter serverActive serverDone = do
    processRequests 0
    putMVar serverDone () -- Notify when the server is done
    putStrLn "Server has stopped."

  where
    -- | Function to process and limitthe requests
    processRequests :: Int -> IO ()
    processRequests count
        | count >= maxReq = do
            --putStrLn "Server has stopped."
            swapMVar serverActive False -- Signal clients to stop adding requests
            putMVar serverDone () -- Notify that the server is done
        | otherwise = do
            req <- readChan requestQueue -- Read a request from the queue
            resTime <- getCurrentTime
            let response = Response (reqestID req) ("Response to " ++ requestContent req) resTime

            -- Write to log file and handle any exceptions
            result <- tryIOError $ appendFile "requests.log" (show (req, response) ++ "\n")
            case result of
                Left (e :: IOException) -> putStrLn $ "Error writing to log file: " ++ show e
                Right _ -> return ()

            -- Increment response counter
            modifyMVar_ responseCounter (\c -> return (c + 1))
            putStrLn $ "Processed request from Client ID: " ++ show (reqestID req)
            processRequests (count + 1)

