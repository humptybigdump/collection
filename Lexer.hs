module Lexer where

data State r = State (Maybe r) (Map Char (State r))
-- data Maybe a = Nothing | Just a
find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:xs)
  | f x = Just x
  | otherwise = find f xs

type Map k v = [(k, v)]

-- 'Just x', falls das Wort 'word' ausgehend
-- vom Startzustand 'state' durch einen finalen Zustand
-- 'S (Just x) _' akzeptiert wird
-- 'Nothing', falls 'word' ausgehend von 'state'
-- nicht akzeptiert wird
accepts :: [Char] -> State r -> Maybe r
accepts [] (State s _)        = s 
accepts (x:xs) (State s map)  | isNothing (lookup x map) = Nothing
                              | otherwise = accepts xs (fromJust (lookup x map))

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Oops, you goofed up, fool."

isJust :: Maybe a -> Bool
isJust (Just a) =  True
isJust Nothing  =  False

isNothing :: Maybe a -> Bool
isNothing =  not . isJust

accepts' :: [Char] -> State r -> Maybe r
accepts' [] (State a trans) = a
accepts' (c:cs) (State a trans) = next (lookup c trans)
  where next (Just s) = accepts cs s
        next Nothing = Nothing

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