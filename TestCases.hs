-- Test cases

import MorseLib
import Morse

-- Q1.1
input1 :: [String]
input1 = ["HELLO WORLD", "3CPO R2D2", "62450875", "THIS IS A LONG SENTENCE CONTAINING NUMBERS 0 12 135"]

output1 :: [[MorseUnit]]
output1 = [[Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence],[Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence],[Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence],[Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence]]

claim1 :: Bool
claim1 = (test1 input1 output1) == True

-- Q1.2
input2 :: [[MorseUnit]]
input2 = output1

output2 :: [String]
output2 = input1

claim2 :: Bool
claim2 = (test2 input2 output2) == True

-- Q1.3
input3 :: [MorseTable]
input3 = table : ((dit ++ dah ++ dit ++ dah ++ dit ++ dah, '.') : (dah ++ dah ++ dit ++ dit ++ dah ++ dah, ',') : table) : []

output3 :: [MorseTree]
output3 = [Branch0 (Branch1 'E' (Branch1 'I' (Branch1 'S' (Branch1 'H' (Leaf '5') (Leaf '4')) (Branch1 'V' Nil (Leaf '3'))) (Branch1 'U' (Leaf 'F') (Branch0 Nil (Leaf '2')))) (Branch1 'A' (Branch1 'R' (Leaf 'L') Nil) (Branch1 'W' (Leaf 'P') (Branch1 'J' Nil (Leaf '1'))))) (Branch1 'T' (Branch1 'N' (Branch1 'D' (Branch1 'B' (Leaf '6') Nil) (Leaf 'X')) (Branch1 'K' (Leaf 'C') (Leaf 'Y'))) (Branch1 'M' (Branch1 'G' (Branch1 'Z' (Leaf '7') Nil) (Leaf 'Q')) (Branch1 'O' (Branch0 (Leaf '8') Nil) (Branch0 (Leaf '9') (Leaf '0'))))),Branch0 (Branch1 'E' (Branch1 'I' (Branch1 'S' (Branch1 'H' (Leaf '5') (Leaf '4')) (Branch1 'V' Nil (Leaf '3'))) (Branch1 'U' (Leaf 'F') (Branch0 Nil (Leaf '2')))) (Branch1 'A' (Branch1 'R' (Leaf 'L') (Branch0 (Branch0 Nil (Leaf '.')) Nil)) (Branch1 'W' (Leaf 'P') (Branch1 'J' Nil (Leaf '1'))))) (Branch1 'T' (Branch1 'N' (Branch1 'D' (Branch1 'B' (Leaf '6') Nil) (Leaf 'X')) (Branch1 'K' (Leaf 'C') (Leaf 'Y'))) (Branch1 'M' (Branch1 'G' (Branch1 'Z' (Leaf '7') (Branch0 Nil (Leaf ','))) (Leaf 'Q')) (Branch1 'O' (Branch0 (Leaf '8') Nil) (Branch0 (Leaf '9') (Leaf '0')))))]

claim3 = (toTree (input3 !! 0) == (output3 !! 0)) && (toTree (input3 !! 1) == (output3 !! 1))

-- Q1.4
input4 :: [MorseTree]
input4 = output3

output4 :: [MorseTable]
output4 = [[([Beep,Silence],'E'),([Beep,Silence,Beep,Silence],'I'),([Beep,Silence,Beep,Silence,Beep,Silence],'S'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'H'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'5'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'4'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'V'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'3'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'U'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'F'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'2'),([Beep,Silence,Beep,Beep,Beep,Silence],'A'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'R'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'L'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'W'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'P'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'J'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'1'),([Beep,Beep,Beep,Silence],'T'),([Beep,Beep,Beep,Silence,Beep,Silence],'N'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'D'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'B'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'6'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'X'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'K'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'C'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'Y'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'M'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'G'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'Z'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'7'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'Q'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'O'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'8'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'9'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'0')],[([Beep,Silence],'E'),([Beep,Silence,Beep,Silence],'I'),([Beep,Silence,Beep,Silence,Beep,Silence],'S'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'H'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'5'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'4'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'V'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'3'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'U'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'F'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'2'),([Beep,Silence,Beep,Beep,Beep,Silence],'A'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'R'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'L'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'.'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'W'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'P'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'J'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'1'),([Beep,Beep,Beep,Silence],'T'),([Beep,Beep,Beep,Silence,Beep,Silence],'N'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'D'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'B'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'6'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'X'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'K'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'C'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'Y'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'M'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'G'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'Z'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'7'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],','),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'Q'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'O'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'8'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'9'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'0')]]

claim4 = ((toTable (input4 !! 0)) `morseTableEquals` (output4 !! 0)) && ((toTable (input4 !! 1)) `morseTableEquals` (output4 !! 1))