import Control.Monad
import System.Environment

import Network.HTTP
import Network.Browser
import WorkerThreads

data Command = Fetch String deriving Show
data Result = Length Int deriving Show

main = do
    [fileName, numThreads] <- getArgs
    urls <- liftM lines (readFile fileName)
    doWork processURL (map Fetch urls) (read numThreads) >>= print
        where processURL (Fetch url) = liftM (Length . length) $
                    simpleHTTP (getRequest url) >>= getResponseBody
