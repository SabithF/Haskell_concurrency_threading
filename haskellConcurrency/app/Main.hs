module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Types
import Client
import Server

main :: IO ()
main = do
    requestQueue <- newMVar []

    -- Initialize the request and response counters
    requestCounter <- newMVar 0
    processedCounter <- newMVar 0

    -- Shared flag to signal server status
    serverActive <- newMVar True 
    serverDone <- newEmptyMVar

    -- Start the server thread
    forkIO $ initServer requestQueue 100 processedCounter serverActive serverDone

    -- Start 10 client threads
    forM_ [1 .. 10] $ \clientId ->
        forkIO $ initClient clientId requestQueue requestCounter serverActive

    -- Wait for the server to finish
    takeMVar serverDone

    -- Print final statistics
    totalRequests <- readMVar requestCounter
    totalResponses <- readMVar processedCounter
    putStrLn $ "Total Requests Added: " ++ show totalRequests
    putStrLn $ "Total Responses Processed: " ++ show totalResponses
    putStrLn "All the requests have been processed successfully."
