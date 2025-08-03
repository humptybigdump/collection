module Hangman where

import Control.Monad (unless)
import Data.Char (isLower)

-- TODOs
-- TODO Hangman Typen entsprechend automaten hinzufügen
data Hangman
  = HangmanRunning
      -- | Secret word.
      String
      -- | List of guesses.
      [Char]
  | HangmanWon String
  | HangmanLost

-- Todo show Funktion für Hangman schreiben
instance Show Hangman where
  show :: Hangman -> String
  show HangmanLost = "You died!\n"
  show (HangmanWon word) = "The word was '" ++ word ++ "'. You win!\n"
  show (HangmanRunning word guesses) = "Wrong characters: " ++ show wrongGuesses ++ "\n" ++ guessedWord ++ "\n" ++ line
    where
      wrongGuesses = [c | c <- guesses, c `notElem` word]
      guessedWord = [if c `elem` guesses then c else '.' | c <- word]
      line = replicate (length word * 2 + 16) '#'

-- TODO: Update Hangman implementieren
updateHangman :: Char -> Hangman -> Hangman
updateHangman guess (HangmanRunning word guesses)
  | guess `elem` guesses = HangmanRunning word guesses
  | hasWon = HangmanWon word
  | hasLost = HangmanLost
  | otherwise = HangmanRunning word (guess : guesses)
  where
    hasWon = all (`elem` (guess : guesses)) word
    hasLost = length (filter (`notElem` word) (guess : guesses)) == 8
updateHangman _ s = s

-- TODO should stop implementieren
shouldStop :: Hangman -> Bool
shouldStop (HangmanRunning _ _) = False
shouldStop _ = True

createGame :: String -> Hangman
createGame s = HangmanRunning s []

main :: IO ()
main = do
  runConsoleGame updateHangman shouldStop (createGame word)
  where
    word = "laziness"

runConsoleGame ::
  -- | How to update game state given an input line.
  (Char -> Hangman -> Hangman) ->
  -- | A predicate for states that stop the game loop.
  (Hangman -> Bool) ->
  -- | Initial game state.
  Hangman ->
  IO ()
runConsoleGame updateGame isFinal = loop
  where
    loop state = do
      putStr $ show state
      unless (isFinal state) $ do
        inputLoop state
    inputLoop state = do
      putStr "\nEnter a lowercase character: "
      input <- getLine
      if isValidInput input then loop (updateGame (head input) state) else inputLoop state

isValidInput :: [Char] -> Bool
isValidInput [a]
  | isLower a = True
isValidInput _ = False