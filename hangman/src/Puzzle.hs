module Puzzle where

--------------------------
-- 13.12 Step Three: Making a puzzle
--------------------------
-- I'm splitting this out for practice with an "external" library
-- and to keep things isolated a little bit

-- put something in the middle of all the things in a list
import Data.List (intersperse)
-- let a program know to stop and that :itsfine:
import System.Exit (exitSuccess)
-- Maybe a -> Bool if Maybe a is Just a
import Data.Maybe (isJust)
-- infinitely loop a thing
import Control.Monad (forever) 

-- Puzzle wordToGuess currentState guessedChars
data Puzzle = Puzzle String [Maybe Char] [Char]

-- Provide a Show instance instead of deriving Show, since
-- we want custom behavior
-- Our show should show the player's current progress toward
-- guessing the word and let them know what they've guessed already
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

-- Create a fresh puzzle from a string
-- Since it's fresh, the wordToGuess is just the input string,
-- nothing has been guessed yet so we have a list of `Nothing`s,
-- and there have been no guesses so the guessedChars is an empty list
-- also, for posterity, this is the point where `Puzzle` stopped
-- looking like a real word for me
freshPuzzle :: String -> Puzzle
freshPuzzle s =
  Puzzle s (map (const Nothing) s) []

-- Check whether guesses are successful
-- elem doesn't care about empty lists, so we don't need to match
-- that case
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wd _ _) c = elem c wd

-- Check whether the player has already guessed a character
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

-- Show a character only if it's been guessed already
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c


-- their fillInCharacter, which I don't like, because `zipper`, to me,
-- doesn't suggest much

--fillInCharacter :: Puzzle -> Char -> Puzzle
--fillInCharacter (Puzzle word filledInSoFar guesses) c =
--  Puzzle word newFilledInSoFar (c : guesses)
--  where
--    zipper guessed wordChar guessChar =
--      if wordChar == guessed
--      then Just wordChar
--      else guessChar
--    newFilledInSoFar =
--      zipWith (zipper c) word filledInSoFar

-- mine, which names the function where we're getting the right
-- locations, and lets the Ord typeclass of Maybe handle extracting
-- the right character
getCorrectLocations:: Puzzle -> Char -> [Maybe Char]
getCorrectLocations (Puzzle w _ _) c =
  map (\x -> if x == c then (Just c) else Nothing) w

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter puzz@(Puzzle word filledInSoFar guesses) c =
  Puzzle word newFilledInSoFar (c : guesses)
  where
    newFilledInSoFar =
      zipWith max (getCorrectLocations puzz c) filledInSoFar

-- handling a guess means taking an input from the user and then doing
-- the right thing
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This charcter was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess)

-- stop the game after a certain number of guesses
-- This is a deliberately stupid / unaware game-end condition, and we'll
-- modify it in exercises
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

-- like game over, but for success
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return()

-- Create the... can I call it an event loop without anyone getting mad
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"
