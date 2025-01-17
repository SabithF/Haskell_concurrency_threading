module Main (main) where

import Control.Concurrent
import Control.Monad (forM_, void)
import Client
import Server

-- | Main function 
main :: IO ()
main = do
    -- Initialize the request queue, new channel, and MVars
    requestQueue <- newChan
    serverActive <- newMVar True
    requestCounter <- newMVar 0 -- Counting the requests
    responseCounter <- newMVar 0 -- Counting the responses processed
    serverDone <- newEmptyMVar

    -- Start the server thread
    _ <- forkIO 
            $ initServer 
                requestQueue 100 requestCounter responseCounter serverActive serverDone

    -- Start 10 client threads
    forM_ [1..10] $ \clientId -> void $ forkIO $ initClient clientId requestQueue serverActive requestCounter


    -- Wait for the server to finish
    takeMVar serverDone
    totalResponses <- readMVar responseCounter

    -- Print the total requests and responses processed
    putStrLn $ "Total Request-Responses Processed: " ++ show totalResponses



-- Commands to run – 
-- “stack build”- to build the project 
-- “stack run +RTS -N4” – to run the project using threads. 
