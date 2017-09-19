module Main where

--------------------------
-- 13.10 Step One: Importing modules
--------------------------
-- it's possible that in real life we won't know this whole list
-- in advance, but we'll play along for now /shrug

-- lowercases a character
import Data.Char (toLower)
-- get random numbers
import System.Random (randomRIO)
import Puzzle

--------------------------
-- 13.11 Step Two: generate a word list
--------------------------

-- for clarity / to register intent
-- type WordList = [String]
-- 13.13 using a newType instead
-- honestly I think this makes things worse since we're not
-- doing anything to enforce more constraints on the type, just
-- adding parentheses in places
newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
--  return (lines dict)
  return $ WordList (lines dict)

validWord :: Int -> Int -> String -> Bool
validWord minLength maxLength s =
  length s >= minLength && length s <= maxLength

-- Good example that we can throw where ... onto anything we want
gameWords :: IO WordList
gameWords = do
--  aw <- allWords
  (WordList aw) <- allWords -- we can bind _into_ data constructors!
--  return $ filter gameWord aw
  return $ WordList (filter gameWord aw)
  where gameWord = validWord minWordLength maxWordLength

-- get a random word from a word list
-- runtime errors on empty lists (!!!), which is consistent with how
-- random.choice works in python standard lib or in numpy.
-- I'm surprised there isn't a safeRandomChoice function or something
getRandomWord :: WordList -> IO String
-- getRandomWord (WordList wl = do
getRandomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- I renamed the above function and this one because `randomWord'` vs.
-- `randomWord` doesn't convey the difference to me as much as these
-- names do
randomWord :: IO String
randomWord = gameWords >>= getRandomWord

main :: IO ()
main = do
  word <- randomWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
