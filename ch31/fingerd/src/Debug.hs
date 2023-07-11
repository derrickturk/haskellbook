module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (sock', _) <- accept sock
  printAndKickback sock'
  close sock'
  where printAndKickback sock = do
          msg <- recv sock 1024
          print msg
          sendAll sock msg

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just $ defaultHints { addrFlags = [AI_PASSIVE] })
    Nothing
    (Just "79")
  let serverAddr = head addrinfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1
  logAndEcho sock
  close sock
