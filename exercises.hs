module Ch13Exercises where

import Data.Char
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.IO

-- Hangman game logic
-- only count wrong guesses
countCorrectGuesses :: String -> [Char] -> Int
countCorrectGuesses s guesses = sum
  $ map (\x -> if elem x s then 1 else 0) guesses

---- stop the game after a certain number of guesses
---- This is a deliberately stupid / unaware game-end condition, and we'll
---- modify it in exercises
--gameOver :: Puzzle -> IO ()
--gameOver (Puzzle wordToGuess _ guessed) =
--  if (length guessed) - countCorrectGuesses wordToGuess guessed > 7 then
--    do putStrLn "You lose!"
--       putStrLn $ "The word was: " ++ wordToGuess
--       exitSuccess
--  else return ()

--------------------------
-- Modifying code
--------------------------

-- 1. Ciphers
-- make them work with user input

----------
-- CIPHERS
----------
-- borrowing Colin's fancy ciphers

newtype Key = Key String

-- | Assumes capital letters.
vigenere :: Key -> String -> String
vigenere (Key key) msg = zipWith encrypt full msg
  where full = concat $ repeat key
        encrypt k c = shift (ord k - 65) c

secretKey :: Key
secretKey = Key "uncrackable"

vigenereFromUser :: IO String
vigenereFromUser = do
  message <- getLine
  return $ vigenere secretKey message

-- | Single-Char caesar cipher.
shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n msg
  | c > 90 = chr $ c - 26
  | otherwise = chr c
  where c = ord msg + n

secretShift :: Int
secretShift = 5

ceasarFromUser :: IO String
ceasarFromUser = do
  message <- getLine
  return $ map ceasar message
  where ceasar = shift secretShift

-- 2. Modify the palindrome block to exit successfully after a False result
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 3. Palindromes on sentences
palindromeSentence :: IO()
palindromeSentence = forever $ do
  line1 <- getLine
  modified <- return $
              filter (\x -> elem x ['a'..'z']) $
              map toLower line1
  case (modified == reverse modified) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 4. Gimme ~Shelter~ Person
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving Show

mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine
  putStr "Please input your age: "
  ageStr <- getLine
  age <- return $ (read ageStr :: Age)
  message <- return $
    case (mkPerson name age) of
      Left e -> show e
      Right person@(Person _ _) ->
        "Got a person! " ++ show person
  putStr message
