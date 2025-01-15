module Client where

import Types
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (when)

-- | Client function to send requests to the server
initClient :: Int -> MVar RequestQueue -> MVar Int -> MVar Bool -> IO ()
initClient clientId queue requestCounter serverActive = clientLoop
  where
    clientLoop = do
        isActive <- readMVar serverActive
        when isActive $ do
            delay <- randomRIO (1, 3) :: IO Int
            threadDelay (delay * 1000000)

            modifyMVar_ queue $ \reqQueueList -> do
                currentTime <- getCurrentTime
                let request = Request clientId ("A request from the client: " ++ show clientId) currentTime
                putStrLn $ "Client " ++ show clientId ++ " added a request"
                return (reqQueueList ++ [request])

            modifyMVar_ requestCounter (\c -> return (c + 1)) -- Increment the request counter
            clientLoop
