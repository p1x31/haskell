{-# LANGUAGE Safe #-} -- For automatic marking to work.

module Assessed1Part1 where

import Data.Char


{- Huffman Codes -}

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

{--- Decoding ---}
-- Notice that this should work for types more general than Char (our c).
-- Question:
{-s - sequence-}
decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (t,s) = decodeAux t t s 

-- You may or may not wish to use a helper function as follows for
-- decode (this function will not be marked, and you can leave it
-- undefined if you don't use it):
decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux (Branch l r i) (Leaf c i') [] = [c]
decodeAux (Branch l r i) (Leaf c i') xs = c:decodeAux (Branch l r i) (Branch l r i) xs 
decodeAux (Branch l r i) (Branch l' r' i') (Z:xs) = decodeAux (Branch l r i) l' xs
decodeAux (Branch l r i) (Branch l' r' i') (I:xs) = decodeAux (Branch l r i) r' xs
{-- decompression --}

{- The input String has the following format:

   * An integer n coded as a sequence of digits.
   
   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}
{-new function get digit-}
{-takes int give string -}
{-
getDigit :: Int -> String
getDigit (x:xs) = if (isInt x) then [x]:(getDigit xs)
else []
-}
{-get tree getbits
dropwhile isDigit take-}
{-getInt :: String -> Int
getInt ("_B":xs) = _-}
parseToInt :: String -> Int
parseToInt xs = (read xs)::Int
--parseToInt xs = [  (digitToInt x) | x <-xs]

getTree :: Read c => String -> Tree c
getTree xs = read(take (parseToInt (fillIntUntilB xs)) (drop (length (fillIntUntilB xs)) xs))

getBitSeq :: String -> String
getBitSeq xs = drop (parseToInt (fillIntUntilB xs)) (drop (length (fillIntUntilB xs)) xs)

fillInt :: String -> String
fillInt xs = [x | x<-xs,(isDigit x)]

fillIntUntilB :: String -> String
fillIntUntilB xs =  takeWhile (/= 'B' ) xs

decompress :: String -> String
decompress xs = decode (getTree xs, getBits(getBitSeq xs))

{-
decompress (xs) = (getDigit x) decode(tree sequence)
-}
{--- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding.  ---}

charlength :: Int
charlength = 8

--gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':xs)  = xs
decompress' xs = decompress xs


{--- Generate the frequency table ---}
--An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

--Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate xs = [(y,count y xs) | y <- rmdup xs]

without :: Eq a => [a] -> a -> [a]
xs `without` y = [x | x <- xs, x /= y]

rmdup :: Eq a => [a] -> [a]
rmdup [] = []
rmdup (x:xs) = x:rmdup(xs `without` x)
{-
frequency :: [c] -> [(c,Int)] 
frequency xs = [(y,count y xs) | y <- rmdup xs]
-}
count :: Eq c => c -> [c] -> Int
count c xs = sum [1 | x <- xs , c == x] 

getBits :: String -> [Bit]
getBits [] = []
getBits ('0':xs) = Z:(getBits xs)
getBits ('1':xs) = I:(getBits xs)
