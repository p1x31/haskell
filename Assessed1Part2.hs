{-# LANGUAGE Safe #-} 
module Assessed1Part2 where

-- We begin with a sample solution of part 1, produced by Cory.

import Data.Maybe
import Data.Char
import Data.List
import Data.Function

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

readBit :: Char -> Bit
readBit '0' = Z
readBit '1' = I

readBits :: [Char] -> [Bit]
readBits = map readBit

decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, bits) = decodeAux tree tree bits

decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux fullTree (Leaf c _) [] = [c]
decodeAux fullTree (Leaf c _) bs = c:(decodeAux fullTree fullTree bs) 
decodeAux fullTree (Branch left right _) (Z:bs) = decodeAux fullTree left bs
decodeAux fullTree (Branch left right _) (I:bs) = decodeAux fullTree right bs

{- The input String has the following format:

   * An integer n coded as a sequence of digits.

   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress str = decode (t,bits)
    where
        (n',str') = span isDigit str
        n         = read n'
        t'        = take n str'
        t         = read t'
        bits      = readBits $ drop n str'

{- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding. -}

charlength :: Int
charlength = 8

-- gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':s)   = s
decompress' s = decompress s

-- Generate the frequency table
-- An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

-- Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate = foldr update []

-- Removes the existing entry for c (if it exists), updates it, and
-- then reinserts it if no entry exists, we start over at 0, and then
-- "update"
update :: Eq c => c -> [Freq c] -> [Freq c]
update c keys = newFreq : rest
    where
        (old,rest) = (is c) `outOf` keys
        key = fromMaybe (c,0) old
        newFreq = mapSnd (+1) key

is :: Eq c => c -> Freq c -> Bool
is c (d,_) = c == d

outOf :: (a -> Bool) -> [a] -> (Maybe a,[a])
outOf p []     = (Nothing,[])
outOf p (x:xs) = if (p x) then (Just x,xs) else (mapSnd (x:) $ outOf p xs)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (c,a) = (c, f a)

{- End of part 1. Your tasks for part 2 begin here. -}

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-- Question:
makeTree :: [Tree c] -> Tree c
-- Collects a list of trees into an optimal prefix tree.
makeTree (x:[]) = x
makeTree (x:y:xs) = makeTree(insertTree (merge x y) xs)

--makeTree (x:xs) = merge x (makeTree xs)
{--}
insertTree :: Tree c-> [Tree c] ->[Tree c]
insertTree t []      = [t] 
insertTree t xs@(x:xs')
  | freq t > freq x = x:(insertTree t xs')
  | otherwise        = t:xs


-- You may wish to use a helper function such as this:
merge :: Tree c -> Tree c -> Tree c
merge (Leaf c1 n1) (Leaf c2 n2)        = (Branch (Leaf c1 n1) (Leaf c2 n2) (n1+n2))
merge (Leaf c n) (Branch l r i)        = (Branch (Leaf c n) (Branch l r i) (n+i))
merge (Branch l r i)(Leaf c n)         = (Branch (Branch l r i) (Leaf c n) (i+n))
merge (Branch l r i) (Branch l' r' i') = (Branch (Branch l r i) (Branch l' r' i') (i+i'))



-- Question:
-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree (map leaf xs)


--generateTree ((c, f):[]) = makeTree ([Leaf c f])
--generateTree ((c, f):xs) = makeTree ([Leaf c f] ++ [generateTree xs])
  --generateTree xs makeTree([Leaf fst(x) snd(x)])
-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Question:
-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
--makeTable (Leaf c i) = [(c,[])]
makeTable (Leaf c i) = toTable [] (Leaf c i)
makeTable (Branch l r i) = toTable [] (Branch l r i)

toTable :: [Bit] -> Tree c -> CodingTable c
toTable xs (Leaf c i )    = [(c,xs)]
toTable xs (Branch l r i) = (toTable (xs++[Z]) l) ++ (toTable (xs++[I]) r)

--makeTable (Branch l r i) = Z:makeTable l ++ I:makeTable r
--getBits :: Tree c -> [Bit]
--getBits (Leaf c i) = []
--getBits (Branch r l i) = map(addBit Z)(getBits l) ++ map (addBit I) (getBits r)

-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable table [] = []
encodeUsingTable table (y:ys) = (lookup' y table) ++ encodeUsingTable table ys

lookup'                  :: (Eq c) => c -> CodingTable c -> [Bit]
lookup'  key []          = []
lookup'  key ((x,y):xys)
    | key == x          =  y
    | otherwise         =  lookup' key xys

-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing t xs = encodeUsingTable(makeTable t) xs

-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode xs = (y, encodeUsing y xs)
           where y = generateTree (sortBy (compare `on` snd) (tabulate xs))
                  
--type Freq c = (c,Int)
quicksort :: (Eq c) => [Freq c] -> [Freq c]  
quicksort [] = []  
quicksort ((c,x):xs) =   
    let smallerSorted = quicksort [(c,y) | (c,y) <- xs, y < x]  
        biggerSorted = quicksort [(c,y) | (c,y) <- xs, y > x]  
    in  smallerSorted ++ [(c,x)] ++ biggerSorted

-- Encoding trees

-- Question:
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress xs = n++t++c
              where n = show (length (show (fst(encode xs))))
                    t = show (fst(encode xs))
                    c = show (snd(encode xs))
-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' xs 
  | (memSize n)+(memSize t)+(length c) < (memSize xs) = compress xs
  | otherwise = ("*"++xs)
  where
     n = show (length t)
     t = show(fst(encode xs))
     c = show (snd(encode xs))
--memSize :: String -> Int
--memSize s = 8 * (length s)
