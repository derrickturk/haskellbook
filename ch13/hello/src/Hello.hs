module Hello
  ( sayHello )
  where

sayHello :: String -> IO ()
sayHello = putStrLn . ("Hi " ++) . (++ "!")
