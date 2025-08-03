module Hangman where

import Control.Monad (unless)
import Data.Char (isLower)

-- TODOs
-- TODO Hangman Typen entsprechend automaten hinzufügen
data Hangman = TODO

-- Todo show Funktion für Hangman schreiben
instance Show Hangman

-- TODO: Update Hangman implementieren
updateHangman :: Char -> Hangman -> Hangman
updateHangman guess TODO = TODO

-- TODO should stop implementieren
shouldStop :: Hangman -> Bool
shouldStop TODO = False

-- TODO create game implementieren
createGame :: String -> Hangman
createGame word = TODO

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