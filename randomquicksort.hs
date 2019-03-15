import System.Random

-- Deterministic quick sort, choosing the first element as the pivot:
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =  qsort [y | y <- xs, y < x] 
             ++ [x] 
             ++ qsort [y | y <- xs, y >= x] 

-- The pivot is the nth element of the list.
getPivot :: [a] -> Int -> (a, [a])
getPivot (x:xs) 0 = (x,xs)
getPivot (x:xs) n = let (p,ys) = getPivot xs (n-1) in (p, x:ys)

-- Now we will choose the pivot randomly, using the Rand monad.

-- A warming-up example:
randomList :: Int -> Rand [Int]
randomList 0 = return []
randomList n = do  
  i <- randInt
  xs <- randomList(n-1)
  return(i : xs)

randomList' :: Int -> [Int]
randomList' n = runRand seed (randomList n)
 where seed = 3 -- say

-- Randomized quicksort using our random monad.

rqsort :: Ord a => [a] -> Rand [a]
rqsort [] = return []
rqsort xs = do
             n <- randIntR (0, length xs - 1)
             let (p, ys) = getPivot xs n
             as <- rqsort [y | y <- ys, y < p] 
             bs <- rqsort [y | y <- ys, y >= p] 
             return(as ++ [p] ++ bs)

rqsort' :: Ord a => [a] -> [a]
rqsort' xs = runRand seed (rqsort xs)
 where seed = 3 -- say

-- The monad code. We could instead have imported a library.

data Rand a = Rand(StdGen -> (a , StdGen))

instance Monad Rand where
 return x = Rand(\g -> (x,g))
 Rand h >>= f = Rand(\g -> let (x, g')   = h g 
                               (Rand h') = f x
                           in h' g')

-- Remove the following to definitions if you have a version of
-- Haskell prior to the monad revolution:

instance Functor Rand where
  fmap f xm = xm >>= pure . f

instance Applicative Rand where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= pure . x

-- Appendix. Random utilities and more examples:

-- Random Int:
randInt :: Rand Int
randInt = Rand random

-- Random Int in a range:
randIntR :: (Int, Int) -> Rand Int
randIntR (lower, upper) = Rand(randomR(lower, upper))

-- This is how we "can get out" of Rand a: 
runRand :: Int -> Rand a -> a
runRand seed (Rand h) = fst(h(mkStdGen seed))

-- Picks a random element with uniform distribution:
uniform :: [a] -> Rand a
uniform xs = do
    n <- randIntR (0, length xs - 1)
    return(xs !! n)

-- Appendix. Merge sort for comparing efficiency.

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (ys,zs) = divide xs in merge (msort ys) (msort zs)

divide :: [a] -> ([a],[a])
divide (a:b:xs) = let (ys,zs) = divide xs in (a:ys,b:zs)
divide xs       = (xs, [])

divide' :: [a] -> ([a],[a])
divide' xs = let l = length xs
                 h = l `div` 2 
             in (take h xs, drop h xs) 

-- Clever trick I learned from Tomas Jakl:
divide'' :: [a] -> ([a],[a])
divide'' xs = f xs xs
 where f (x:xs) (_:_:ys) = let (as,bs) = f xs ys in (x:as, bs) 
       f     xs       _  = ([], xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (a:xs) (b:ys)
  | a <= b = a : merge xs (b:ys)
  | b <  a = b : merge (a:xs) ys
