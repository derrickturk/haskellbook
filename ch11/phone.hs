-- a super inefficient encoding
-- also this whole fucking exercise was worthless because
--  "Big" has an ambiguous decoding (*224444) -> 'B4' or 'Bhh' or 'Big' ...

import Data.Char (toLower, toUpper, isAsciiUpper)
import Data.List (nub, lookup, find)

type Key = Char

data KeyMap = KeyMap { capsKey :: Key, standardKeys :: [(Key, [Char])] }
  deriving Show

data KeyPress = NoCapsPrefix Key Int
              | CapsPrefix Key Int
  deriving Show

standardMap :: KeyMap
standardMap = KeyMap {
  capsKey = '*',
  standardKeys = [
    ('1', "")
  , ('2', "abc")
  , ('3', "def")
  , ('4', "ghi")
  , ('5', "jkl")
  , ('6', "mno")
  , ('7', "pqrs")
  , ('8', "tuv")
  , ('9', "wxyz")
  , ('0', " ")
  , ('#', ".,")
  ]
}

outputCharMap :: KeyMap -> [(Key, [Char])]
outputCharMap = map (\(k, cs) -> (k, cs ++ [k])) . standardKeys

validChar :: KeyMap -> Char -> Bool
validChar km = flip elem (concatMap snd $ outputCharMap km) . toLower

toRuns :: [Key] -> [(Key, Int)]
toRuns [] = []
toRuns l@(k:_) = (k, length $ takeWhile (== k) l):(toRuns $ dropWhile (== k) l)

fromRuns :: [(Key, Int)] -> [Key]
fromRuns [] = []
fromRuns ((k, n):rest) = (take n $ repeat k) ++ fromRuns rest

toCaps :: KeyPress -> KeyPress
toCaps (NoCapsPrefix k n) = CapsPrefix k n
toCaps kp = kp

-- this was hard as fuck but this implementation rejects invalid
--   caps-prefix sequences
toPresses :: KeyMap -> [(Key, Int)] -> Maybe [KeyPress]
toPresses km = sequence . toPresses' where
  toPresses' :: [(Key, Int)] -> [Maybe KeyPress]
  toPresses' [] = []
  toPresses' ((k, n):rest)
    | k == capsKey km && n /= 1 = [Nothing]
    | k == capsKey km = case rest of
        (k, n):rest -> (Just $ CapsPrefix k n):(toPresses' rest)
        _ -> [Nothing]
    | otherwise = (Just $ NoCapsPrefix k n):(toPresses' rest)

fromPresses :: KeyMap -> [KeyPress] -> [(Key, Int)]
fromPresses _ [] = []
fromPresses km ((CapsPrefix k n):rest) =
  (capsKey km, 1):(fromPresses km $ (NoCapsPrefix k n):rest)
fromPresses km ((NoCapsPrefix k n):rest) = (k, n):(fromPresses km rest)

decodeKey :: KeyMap -> KeyPress -> Maybe Char
decodeKey km (CapsPrefix k n) = toUpper <$> decodeKey km (NoCapsPrefix k n)
decodeKey km (NoCapsPrefix k n) = (decode n) <$> lookup k (outputCharMap km)
  where decode n cs = cs !! mod (n - 1) (length cs)

decodeKeys :: KeyMap -> [Key] -> Maybe String
decodeKeys km keys = case toPresses km $ toRuns keys of
  Nothing -> Nothing
  Just presses -> sequence $ map (decodeKey km) presses

-- one day I will pointfree this motherfucker
-- decodeKeys km = ((sequence . map (decodeKey km)) <$>) . (toPresses km . toRuns)

encodeChar :: KeyMap -> Char -> Maybe KeyPress
encodeChar km c = case find (elem (toLower c) . snd) $ outputCharMap km of
  Nothing -> Nothing
  Just (k, cs) -> Just $ encode c k cs
  where encode c k cs
          | isAsciiUpper c = toCaps $ encode (toLower c) k cs
          | otherwise = NoCapsPrefix k ((length $ takeWhile (/= c) cs) + 1)

{--
encodeChars :: KeyMap -> String -> Maybe [Key]
encodeChars km s = case sequence $ map (encodeChar km) s of
  Nothing -> Nothing
  Just presses -> Just $ fromRuns $ fromPresses km presses
--}

encodeChars :: KeyMap -> String -> Maybe [Key]
encodeChars km =
  ((fromRuns . fromPresses km) <$>) . sequence . map (encodeChar km)
