module Main where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

getUrls :: IO [Response ByteString]
getUrls = traverse get urls

main :: IO ()
main = return ()
