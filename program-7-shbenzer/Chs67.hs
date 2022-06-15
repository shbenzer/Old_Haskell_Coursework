module Chs67 where

--don't delete the import, obviously
--remember to include function types (3 points each)


sumdown :: Int -> Int;
sumdown 0 = 0;
sumdown x = x + sumdown(x-1);



euclid :: Int -> Int -> Int;
euclid x y | x == y = x
           | y < x = euclid (x-y) y
           | x < y = euclid (y-x) x;
--euclid _ _ = 0;


sum' :: [Int] -> Int;
sum' [] = 0;
sum' (x:xs) = x + sum' xs;


take' :: Integral b => b -> [a] -> [a]
take' 0 _= [];
take' n (x:xs) = [x] ++ take' (n-1) xs;


last' :: [a] -> a
last' (x:[]) = x;
last' (x:xs) = last' xs;

--similar to bin2int in book?

dec2int' :: [Int] -> Int;
dec2int' _ = 0;


altmap :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap f g [] = [];
altmap f g (x:y:zs) = f x : g y : altmap f g zs;

--define your own function(s) to help with luhn

luhn :: [Int] -> Bool;
luhn x | sum (subtract9Luhn (doubleLuhn x)) `mod` 10 == 0 = True
       | sum (subtract9Luhn (doubleLuhn x)) `mod` 10 /= 0 = False;

doubleLuhn :: [Int] -> [Int];
doubleLuhn (x:y:zs) = [x] ++ [2*y] ++ doubleLuhn(zs);
doubleLuhn (x:[]) = [x];
doubleLuhn [] = [];

subtract9Luhn :: [Int] -> [Int];
subtract9Luhn (x:xs) | x > 9 = [x-9] ++ subtract9Luhn xs
                     | x <= 9 = [x] ++ subtract9Luhn xs;


