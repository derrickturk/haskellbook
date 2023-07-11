module Ch13 where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit (exitSuccess)

canonicalize :: String -> String
canonicalize = concat . words . map toLower

palindrome :: String -> Bool
palindrome s = canonical == reverse canonical where
  canonical = canonicalize s 

runPalindromes :: IO ()
runPalindromes = forever $ do
  l <- getLine
  if palindrome l
    then putStrLn "palindrome"
    else putStrLn "not palindrome" >> exitSuccess

-- below copied from book

type Name = String

type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Name: "
  name <- getLine
  putStrLn "Age: "
  age <- read <$> getLine
  case mkPerson name age of
    Left err -> putStrLn $ "Invalid: " ++ show err
    Right person -> (putStrLn "Got a person!") >>
                    (putStrLn $ show person)
