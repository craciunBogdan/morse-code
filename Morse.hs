module Morse where

import MorseLib
-- import qualified Debug.Trace




{- *** Exercise 1 *** -}




-- Function that encodes a word.
codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (c:cs) = (codeSymbol c) ++ shortGap ++ (codeWord cs)

-- Function that encodes a list of words.
codeText :: [String] -> [MorseUnit]
codeText []     = []
codeText (w:[]) = (codeWord w) ++ (codeText [])
codeText (w:ws) = (codeWord w) ++ mediumGap ++ (codeText ws)

-- Function that encodes a full string.
encode :: String -> [MorseUnit]
encode s = codeText (words s)




{- *** Exercise 2 *** -}




-- Check if a MorseUnit list is a character in the MorseLib table.
decodeChar' :: MorseLib.MorseTable -> [MorseUnit] -> Char
decodeChar' (e:es) c = if c == (fst e) 
                       then (snd e) 
                       else decodeChar' es c

-- Wrapper for decodeChar'.
decodeChar :: [MorseUnit] -> Char
decodeChar xs = decodeChar' MorseLib.table xs

-- Get all the MorseUnits out of a MorseUnit list that appear before the first shortGap. Returns a pair containing the list of MorseUnits that were found before the first shortGap and the remaining MorseUnits after the first shortGap from the original list.
getNextChar' :: [MorseUnit] -> [MorseUnit] -> ([MorseUnit], [MorseUnit])
getNextChar' []           c = (c, [])
getNextChar' (Silence:xs) c = if (last c) == Silence
                              then (c, tail xs)
                              else getNextChar' xs (c ++ [Silence])
getNextChar' (Beep:xs)    c = getNextChar' xs (c ++ [Beep])

-- Wrapper for getNextChar'.
getNextChar :: [MorseUnit] -> ([MorseUnit], [MorseUnit])
getNextChar xs = getNextChar' xs []

-- Function that decodes a word.
decodeWord :: [MorseUnit] -> String
decodeWord [] = []
decodeWord xs = [decodeChar (fst x)] ++ decodeWord (snd x)
            where
                x = getNextChar xs

-- Get all the MorseUnits out of a MorseUnit list that appear before the first mediumGap. Returns a pair containing the list of MorseUnits that were found before the first mediumGap and the remaining MorseUnits after the first mediumGap from the original list.
getNextWord' :: [MorseUnit] -> [MorseUnit] -> ([MorseUnit], [MorseUnit])
getNextWord' []           w = (w, [])
getNextWord' (Beep:xs)    w = getNextWord' xs (w ++ [Beep])
getNextWord' (Silence:xs) w = if ((last w == Silence)                 &&
                                  (last (init w) == Silence)          &&
                                  (last (init (init w)) == Silence))
                              then (w, tail (tail (tail xs)))
                              else getNextWord' xs (w ++ [Silence])

-- Wrapper for getNextWord'
getNextWord :: [MorseUnit] -> ([MorseUnit], [MorseUnit])
getNextWord xs = getNextWord' xs []

-- Decode a whole list of MorseUnits.
decode' :: [MorseUnit] -> String -> String
decode' [] s = s
decode' xs s = decode' xs' s' 
            where
                xs' = snd (getNextWord xs)
                s'  = if (length xs' > 0)
                      then s ++ decodeWord (fst (getNextWord xs)) ++ " "
                      else s ++ decodeWord (fst (getNextWord xs))

-- Wrapper for decode'
decode :: [MorseUnit] -> String
decode xs = decode' xs []




{- *** Exercise 3 *** -}




-- Function that takes a list of pairs as an argument and returns a list of the first elements of each pair in the original list.
fsts ::[(a,b)] -> [a]
fsts xs = map fst xs

-- Function that takes a list of pairs as an argument and an element whose first occurence in the original list as the first element of a pair will be removed.
removeFirstInstance :: Eq a => [(a,b)] -> a -> [(a,b)]
removeFirstInstance []     a = []
removeFirstInstance (x:xs) a = if (fst x == a) 
                               then xs 
                               else x : (removeFirstInstance xs a)

-- Function that takes a list of pairs as the first argument and another element of the same type as the first elements of the pairs in the list as a second argument. The function searches for the first occurence of the second argument in the list as the first element of a pair and returns the second element of said pair.
getSndOfFirstOccurence :: Eq a => [(a,b)] -> a -> b
getSndOfFirstOccurence []     a = undefined
getSndOfFirstOccurence (x:xs) a = if (fst x == a)
                                  then (snd x)
                                  else getSndOfFirstOccurence xs a

-- Function that transforms a MorseTable into a MorseTree.
toTree :: MorseTable -> MorseTree
toTree []        = Nil
toTree [([], c)] = Leaf c
toTree xss       | [] `elem` (fsts xss) = Branch1 (getSndOfFirstOccurence xss []) (toTree yss) (toTree zss)
                 | otherwise            = Branch0 (toTree yss) (toTree zss)
    where
        yss = [((drop 2 (fst ys)), snd ys) | ys <- (removeFirstInstance xss []), ((head (fst ys)) : ((fst ys) !! 1) : []) == dit]
        zss = [((drop 4 (fst zs)), snd zs) | zs <- (removeFirstInstance xss []), ((head (fst zs)) : ((fst zs) !! 1) : ((fst zs) !! 2) : ((fst zs) !! 3) : []) == dah]




{- *** Exercise 4 *** -}




-- Function that transforms a table into a tree.
toTable' :: MorseTree -> [MorseUnit] -> MorseTable
toTable' (Nil)             us = []
toTable' (Leaf c)          us = [(us, c)]
toTable' (Branch0 t1 t2)   us = (toTable' t1 (us ++ dit)) ++ (toTable' t2 (us ++ dah))
toTable' (Branch1 c t1 t2) us = [(us, c)] ++ (toTable' t1 (us ++ dit)) ++ (toTable' t2 (us ++ dah))

-- Wrapper for toTable'.
toTable :: MorseTree -> MorseTable
toTable t = toTable' t []




{- End of exercises. -}




-- DEBUGGING & TESTING.

-- Function that takes a list of strings and a list of MorseUnit lists. It then encodes each string and tests if it is equal to its respective MorseUnit list.
test1 :: [String] -> [[MorseUnit]] -> Bool
test1 [] []         = True
test1 [] _          = False
test1 _  []         = False
test1 (i:is) (o:os) = if ((encode i) == o) 
                      then (test1 is os) 
                      else False

-- Function that takes a list of MorseUnit lists and a list of strings. It then decodes each MorseUnit list and tests if it is equal to its respective string.
test2 :: [[MorseUnit]] -> [String] -> Bool
test2 [] []         = True
test2 [] _          = False
test2 _  []         = False
test2 (i:is) (o:os) = if ((decode i) == o) 
                      then (test2 is os) 
                      else False

-- Function that takes a list as the first argument and an element as the second argument. It then counts how many times the second argument appears in the first argument.
count :: Eq a => a -> [a] -> Int
count a []     = 0
count a (x:xs) = if (a == x) 
                 then (1 + (count a xs)) 
                 else (count a xs)

-- Function that takes a list as the first argument and an element as the second argument. It the checks to see if the second argument appears in the first argument.
contains :: Eq a => [a] -> a -> Bool
contains xs x = if (count x xs) > 0 
                then True 
                else False

-- Function that checks the equality of two MorseTables disregarding the order of the elements of the tables.
morseTableEquals :: MorseTable -> MorseTable -> Bool
morseTableEquals [] t2s       = True
morseTableEquals (t1:t1s) t2s = if (t2s `contains` t1) 
                                then morseTableEquals t1s t2s 
                                else False