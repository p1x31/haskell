--f :: [a] -> [a] 
--f xs = reverse(f xs)
--xs = f xs
--xs ++ f xs = f(xs ++ reverse xs)
--length(f xs) = (length xs - 1) `max` 0
--f xs = []
--f [] = []
--f (x:xs) = [x] ++ f xs same 
--f [x] = []
--f(x:xs)= x:f(xs) del last
--f(x:xs) = f xs ++ [x] reverse
--f (x:xs) = [x] ++ f xs ++ [x] clone in reverse e
--f (x:y:zs) = y:f(x:zs) hhzhz exeption
--f (x:y:zs) = f(x:xs) ++ [y] exception
data Tree = Nil | Fork Tree Tree

f :: Int -> Tree
f n | n == 1 = Nil
     | even n = Fork (f(n `div` 2)) (f(n `div` 2))
     | otherwise = Fork(f(3*n+1))(f(3*n+1))
     
--data Tree a = Fork a [Tree a]     
--count :: Treea a -> Int
--count (Fork x forest) = 1 + sum(map count forest)
--count (Fork _ []) = 1
--count (Fork _ (x:xs)) = count x + sum (map count xs)
count :: Tree -> Int
count Nil = 0
count (Fork l r) = 1 + count l + count r
