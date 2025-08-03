module Lexer where

data State r = State (Maybe r) (Map Char (State r))

type Map k v = [(k, v)]

-- 'Just x', falls das Wort 'word' ausgehend
-- vom Startzustand 'state' durch einen finalen Zustand
-- 'S (Just x) _' akzeptiert wird
-- 'Nothing', falls 'word' ausgehend von 'state'
-- nicht akzeptiert wird
accepts :: [Char] -> State r -> Maybe r
accepts word state = _

-- Zum Testen, Automat der Zurückgibt in welchem Zahlensystem
-- man sich befindet, Siehe NumberSysAutomata.PNG
data NumberSystem = Zero | Bin | Dec | Oct

automata :: State NumberSystem
automata = s
  where
    s = State Nothing ([(d, s10) | d <- ['1' .. '9']] ++ [('0', s0)])
    s0 = State (Just Zero) ([(d, s8) | d <- ['0' .. '7']] ++ [('b', s2)])
    s8 = State (Just Oct) [(d, s8) | d <- ['0' .. '7']]
    s2 = State (Just Bin) [('0', s2), ('1', s2)]
    s10 = State (Just Dec) [(d, s10) | d <- ['0' .. '9']]