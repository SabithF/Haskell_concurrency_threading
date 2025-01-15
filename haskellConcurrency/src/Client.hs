module Client where 

import Types
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (forever)



-- | client funtion to send request to the server 
initClient :: Int -> MVar RequestQueue -> IO ()
initClient clientId queue =  forever $ do
    delay <- randomRIO (1, 3) :: IO Int 
    threadDelay (delay * 1000000)

    -- creating a request and sending
    currentTime <- getCurrentTime
    let request = Request clientId ("A request from the client :" ++ show clientId ) currentTime

    reqQueueList <- takeMVar queue
    putMVar queue (reqQueueList ++ [request])

    putStrLn $ "A request from the Client " ++ show clientId