module Ch27 where

good :: IO ()
good = do
  let x :: Integer
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

bad :: IO ()
bad = do
  let x :: Integer
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _ -> putStrLn "hello"
