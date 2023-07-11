module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  hPutStr h $ replicate 100000000 '0' ++ "abc"
  hClose h

data PleaseDie = PleaseDie deriving Show

instance Exception PleaseDie

-- this doesn't work because they don't have main wait on
--   the child thread
main :: IO ()
main = do
  -- threadId <- forkIO openAndWrite
  threadId <- forkIO $ mask_ openAndWrite
  threadDelay 1000
  throwTo threadId PleaseDie
