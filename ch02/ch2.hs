module Ch2 where

sayHello :: String -> IO ()
sayHello = putStrLn . ("Hello, " ++) . (++ "!")

circleArea :: (Floating a) => a -> a
circleArea = (* pi) . (^ 2)

thing = x * 3 + y
    where x = 3
          y = 1000

main :: IO ()
main = sayHello "world"
