 module WorkerThreads (doWork) where

import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

doWork :: (a -> IO b) ->  [a] -> Int -> IO [b]
doWork f initialCommands numWorkers = do
    commands <- newChan
    results <- newChan
    spawnWorkers f commands results numWorkers
    writeList2Chan commands initialCommands
    processedResults <- getChanContents results
    return $ take (length initialCommands) processedResults

spawnWorkers :: (a -> IO b) -> Chan a -> Chan b -> Int -> IO [ThreadId]
spawnWorkers f c r num = sequence [forkIO (worker f c r) | _ <- [1..num]]

worker :: (a -> IO b) -> Chan a -> Chan b -> IO ()
worker f commands results = forever $ do
        command <- readChan commands
        result <- f command
        writeChan results result
