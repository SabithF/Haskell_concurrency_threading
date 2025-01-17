module Client (
    initClient
) where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Random (randomRIO)
import Types
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime)

-- | Function to initialize the clients
initClient :: Int -> Chan Request -> MVar Bool -> MVar Int -> IO ()
initClient clientId requestQueue serverActive requestCounter = forever $ do
    active <- readMVar serverActive
    if not active
        then return ()  -- Stop the client loop if server is inactive
        else do
            -- Adding random time interval between requests
            delay <- randomRIO (1, 3) :: IO Int
            threadDelay (delay * 1000000)

            -- Add a new request only if the limit is not exceeded
            reqCount <- readMVar requestCounter
            if reqCount >= 100
                then do
                    swapMVar serverActive False
                    return ()  -- Stop the client if request limit reached
                else do
                    reqTime <- getCurrentTime
                    let request = Request clientId ("Request from Client " ++ show clientId) reqTime
                    writeChan requestQueue request
                    modifyMVar_ requestCounter (\c -> return (c + 1))
                    putStrLn $ "Client " ++ show clientId ++ " sent a request."
