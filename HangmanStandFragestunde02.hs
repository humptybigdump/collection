module Hangman where

import Control.Monad (unless)
import Data.Char (isLower)

-- TODOs
-- TODO Hangman Typen entsprechend automaten hinzufügen
data Hangman = Hangman String String String | Lost | Won String

-- Todo show Funktion für Hangman schreiben
instance Show Hangman where
  show Lost = "You lost the game"
  show (Won s) = "You saved him, the word was: " ++ s
  show (Hangman word correctGuess falseGuess) = hide ++ "\nWrong characters: " ++ falseGuess
    where
      hide = map (\x -> if x `elem` correctGuess then x else "_") word

-- TODO: Update Hangman implementieren
updateHangman :: Char -> Hangman -> Hangman
updateHangman guess Lost = Lost
updateHangman guess (Won s) = Won s
updateHangman guess (Hangman word correctGuess falseGuess)
  | elem guess (correctGuess ++ falseGuess) = Hangman word correctGuess falseGuess
  | elem guess word && win = Won word
  | elem guess word = Hangman word (guess:correctGuess) falseGuess
  | lost = Lost
  | otherwise = Hangman word correctGuess (guess:falseGuess)
  where
    win = foldr (\c o -> o && c `elem` (guess : correctGuess)) True word
    lost = length falseGuess + 1 == 8

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