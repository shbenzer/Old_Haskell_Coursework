module Chs45 where

-- Problem 4.1
halve :: [a] -> ([a],[a])
halve xs = ([], [])
halve xs = (x,s) 
    where
        x = [a | a <- take ((length xs) `div` 2) xs]
        s = [b | b <- take ((length xs) `div` 2) (reverse xs)]

    
-- Problem 4.2 a with head and tail
third :: [a] -> a
third xs = head (tail (tail xs))

-- Problem 4.2 b with indexing !!
third' :: [a] -> a
third' xs = xs !! 2

-- Problem 4.2 c with pattern matching
third'' :: [a] -> a
third'' (_:_:x:_) = x

-- Problem 5.2
grid :: Int -> Int -> [(Int,Int)]
grid x y = [(m,n) | m <- [0..x], n <- [0..y]]

-- Problem 5.3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Problem 5.4
replicate' :: Int -> a -> [a]
replicate' i x = [a | a <- x, length a > i]
