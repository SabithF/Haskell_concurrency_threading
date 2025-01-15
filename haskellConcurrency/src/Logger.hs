module Logger where 

import System.IO
import Types

-- | Function to write the log file

writeLogger ::  [(Request, Response)] -> IO ()
writeLogger logs = withFile "requests.log" WriteMode $ \handle -> do
    mapM_ (hPrint handle) logs