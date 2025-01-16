module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Client
import Server

main :: IO ()
main = do
    -- Shared resources
    requestQueue <- newChan
    serverActive <- newMVar True
    requestCounter <- newMVar 0 -- Counting the requests
    responseCounter <- newMVar 0 -- Counting the responses processed
    stopSignal <- newEmptyMVar  -- New MVar to signal clients to stop
    serverDone <- newEmptyMVar

    -- Start the server thread
    forkIO $ initServer requestQueue 100 requestCounter responseCounter serverActive serverDone

    -- Start 10 client threads
    forM_ [1..10] $ \clientId -> void $ forkIO $ initClient clientId requestQueue serverActive requestCounter


    -- Wait for the server to finish
    takeMVar serverDone
    totalRequests <- readMVar requestCounter
    totalResponses <- readMVar responseCounter

    -- Print the total requests and responses processed
    putStrLn $ "Total Requests Added: " ++ show totalRequests
    putStrLn $ "Total Responses Processed: " ++ show totalResponses
