module MorseSample where

import MorseLib

-- Q1.1
encode :: String -> [MorseUnit]
encode s = codeText (words s)

codeWord :: String -> [MorseUnit]
codeWord w = concat [codeSymbol c ++ shortGap | c <- w]

codeText :: [String] -> [MorseUnit]
codeText [] = []
codeText [w] = codeWord w
codeText (w:ws) = codeWord w ++ mediumGap ++ codeText ws

-- Q1.2
decode :: [MorseUnit] -> String
decode [] = ""
decode xss = x ++ decode (drop n xss)
  where
    [(x, n)] = [(c : s, len + num) | (m, c) <- table, let len = length m, take len xss == m, let (hitSilence, num, s) = decodeHelper (drop len xss), hitSilence]

decodeHelper :: [MorseUnit] -> (Bool, Int, String)
decodeHelper (Silence:Silence:Silence:Silence:Silence:Silence:xs) = (True, 6, " ")
decodeHelper (Silence:Silence:xs) = (True, 2, "")
decodeHelper [] = (True, 0, "")
decodeHelper _ = (False, 0, "")

-- alternative solution
table' :: MorseTable
table' = (mediumGap, ' ') : [(m ++ shortGap, c) | (m, c) <- table]

decode' :: [MorseUnit] -> String
decode' [] = ""
decode' xss = x : decode' (drop n xss)
  where
    [(x, n)] = [(c, len) | (m, c) <- table', let len = length m, take len xss == m]

-- Q1.3
toTree :: MorseTable -> MorseTree
toTree [] = Nil
toTree [([], c)] = Leaf c
toTree t = case cs of
             []  -> Branch0 (toTree zss) (toTree yss)
             [c] -> Branch1 c (toTree zss) (toTree yss)
  where
    cs = [c | ([], c) <- t]
    yss = takeDah t
    zss = takeDit t

takeDit :: MorseTable -> MorseTable
takeDit t = [(drop 2 m, c) | (m, c) <- t, take 2 m == dit]

takeDah :: MorseTable -> MorseTable
takeDah t = [(drop 4 m, c) | (m, c) <- t, take 4 m == dah]

-- Q1.4
toTable :: MorseTree -> MorseTable
toTable Nil = []
toTable (Leaf c) = [([], c)]
toTable (Branch0 l r) = map (\(m, c) -> (dit ++ m, c)) (toTable l) ++ map (\(m, c) -> (dah ++ m, c)) (toTable r)
toTable (Branch1 c l r) = [([], c)] ++ map (\(m, c) -> (dit ++ m, c)) (toTable l) ++ map (\(m, c) -> (dah ++ m, c)) (toTable r)

-- test if elements of two lists are equal
testListElem :: Eq a => [a] -> [a] -> Bool
testListElem [] [] = True
testListElem (x:xs) m
  = case helper x m of
      []  -> False
      [y] -> testListElem xs [z | z <- m, z /= y]
  where
    helper x l = [y | y <- l, x == y]





