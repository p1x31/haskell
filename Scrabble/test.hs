getNumbers :: Int -> [Int]
getNumbers n = xs ++ [n] ++ (reverse xs)
  where
    xs = [0..n-1]

myGetLine :: Int -> Int -> String
myGetLine n m = spaces ++ (foldl myConcat (show (head xs)) (tail xs))
  where
    spaces = replicate (2*(n-m)) ' '
    myConcat s x = s ++ " " ++ show x
    xs = getNumbers m

solve :: Int -> IO ()
solve n = sequence_ (map printLine (getNumbers n))
  where
    printLine :: Int -> IO ()
    printLine m = putStrLn (myGetLine n m)

main :: IO ()
main = readLn >>= solve

