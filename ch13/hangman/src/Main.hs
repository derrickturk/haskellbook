module Main where

import Control.Monad (forever, when)
import Data.Char (toLower, isAsciiLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse, sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))

type WordList = [String]

-- fmap == liftM
allWords :: IO WordList
allWords = fmap lines $ readFile "data/dict.txt"

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allowedBadGuesses :: Int
allowedBadGuesses = 7

gameWords :: IO WordList
gameWords = fmap (filter inBounds) allWords where
  inBounds w
    | length w < minWordLength = False
    | length w > maxWordLength = False
    | otherwise = True

randomWord :: IO String
randomWord = do
  words <- gameWords
  idx <- randomRIO (0, length words - 1)
  return $ words !! idx

--                             word   correct      guesses bad guesses
data PuzzleState = PuzzleState String [Maybe Char] [Char] Int

instance Show PuzzleState where
  show (PuzzleState _ correct guesses bad) =
    (intersperse ' ' $ map (fromMaybe '_') correct) ++
    " | guessed so far: " ++ sort guesses ++
    " | " ++ show bad ++ " bad guesses"

makePuzzle :: String -> PuzzleState
makePuzzle word = PuzzleState word (map (const Nothing) word) "" 0

applyGuess :: PuzzleState -> Char -> PuzzleState
applyGuess current@(PuzzleState word correct guesses bad) c
  | c `elem` guesses = current
  | otherwise = PuzzleState
                  word
                  (map (toCorrect (c:guesses)) word)
                  (c:guesses)
                  badGuesses
      where toCorrect gs c
              | c `elem` gs = Just c
              | otherwise = Nothing
            badGuesses = if c `elem` word then bad else bad + 1

{--
checkLoseGame :: PuzzleState -> IO ()
checkLoseGame (PuzzleState w _ _ bad)
  | bad > allowedBadGuesses = putStrLn "You lose!" >>
                              (putStrLn $ "The word was: " ++ w) >>
                              exitSuccess
  | otherwise = return ()

checkWinGame :: PuzzleState -> IO ()
checkWinGame (PuzzleState _ correct _ _)
  | all isJust correct = putStrLn "You win!" >> exitSuccess
  | otherwise = return ()
--}

checkWinGame (PuzzleState _ correct _ _) = when (all isJust correct) $
  putStrLn "You win!" >> exitSuccess

checkLoseGame (PuzzleState w _ _ bad) = when (bad > allowedBadGuesses) $
  putStrLn "You lose!" >>
  (putStrLn $ "The word was: " ++ w) >>
  exitSuccess

runGame :: PuzzleState -> IO ()
runGame puzzle = forever $ do
  print puzzle
  checkLoseGame puzzle
  checkWinGame puzzle
  putStr "Guess: "
  guess <- getLine
  case guess of
    [c] | isAsciiLower c -> runGame $ applyGuess puzzle c
    _ -> putStrLn "Bad guess."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord
  runGame $ makePuzzle (map toLower word)
