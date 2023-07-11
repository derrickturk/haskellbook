module Main where

import System.Environment (getProgName, getArgs)
import System.IO (Handle, hIsEOF, hPutStrLn, hGetLine, stdout, stdin, stderr)
import Control.Monad (when)

import Vignere

main :: IO ()
main = do
  args <- getArgs
  case args of
    [key, "-d"] -> decrypt key
    [key, "-e"] -> encrypt key
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "Usage: " ++ prog ++ " <key> -[d|e]"

tilEOF :: Handle -> IO a -> IO ()
tilEOF h m = do
  isEOF <- hIsEOF h
  when (not isEOF) $ m >> tilEOF h m

encrypt :: String -> IO ()
encrypt key = tilEOF stdin $
  hGetLine stdin >>= (hPutStrLn stdout) . (vignere key)

decrypt :: String -> IO ()
decrypt key = tilEOF stdin $
  hGetLine stdin >>= (hPutStrLn stdout) . (unVignere key)
