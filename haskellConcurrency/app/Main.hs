module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Types
import Client
import Server


main :: IO ()
main = do
    -- | Creating a request queue
    requestQueue <- newMVar []

    -- | Signaling the server termination
    serverDone <- newEmptyMVar

    -- | Creating a server thread
    forkIO $ initServer requestQueue 10 serverDone

    -- | Creating 10 client threads
    forM_ [1..10] $ \clientId -> forkIO $ initClient clientId requestQueue

    -- | Waiting for the server to end
    takeMVar serverDone

    putStrLn "All the requests have been processed successfully"

