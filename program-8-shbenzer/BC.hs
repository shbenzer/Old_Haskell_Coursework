module BC where

--indexInto returns the index of the first argument in a list 
--(don't worry about error checking -- can assume in list)
indexInto :: Eq a => a -> [a] -> Int
indexInto x ([]) = -1;
indexInto x (y:ys) | x == y = 0
                   | x /= y = 1 + indexInto x (ys);

--converts a character into its corresponding integer value
-- e.g. '0' to 0, 'A' to 10, 'Z' to 35 
-- like hex, except with more digits
-- (consider using elem -- look it up)
alphabet = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];
dig2Int :: Char -> Int
dig2Int dChar | elem dChar (alphabet) == False = 0
              | elem dChar (alphabet) == True = indexInto dChar (alphabet);

--converts an integer in range 0..35 into its 
-- corresponding digit (0,1..Z)
-- again, don't care about ints out of bounds
num2char :: Int -> Char
num2char n | n == 0 = '0'
           | n == 1 = '1'
           | n == 2 = '2'
           | n == 3 = '3'
           | n == 4 = '4'
           | n == 5 = '5'
           | n == 6 = '6'
           | n == 7 = '7'
           | n == 8 = '8'
           | n == 9 = '9'
           | n == 10 = 'A'
           | n == 11 = 'B'
           | n == 12 = 'C'
           | n == 13 = 'D'
           | n == 14 = 'E'
           | n == 15 = 'F'
           | n == 16 = 'G'
           | n == 17 = 'H'
           | n == 18 = 'I'
           | n == 19 = 'J'
           | n == 20 = 'K'
           | n == 21 = 'L'
           | n == 22 = 'M'
           | n == 23 = 'N'
           | n == 24 = 'O'
           | n == 25 = 'P'
           | n == 26 = 'Q'
           | n == 27 = 'R'
           | n == 28 = 'S'
           | n == 29 = 'T'
           | n == 30 = 'U'
           | n == 31 = 'V'
           | n == 32 = 'W'
           | n == 33 = 'X'
           | n == 34 = 'Y'
           | n == 35 = 'Z';
--converts an integer value to a string representing
-- the number in base b
-- suggest looking up repeated division strategy
-- for how to convert base 10 to binary and 
-- then generalize
int2BaseHelper :: Int -> Int -> String
int2BaseHelper n b |n == 0 = ""
             | n /= 0 = int2Base (n `div` b) b ++ show(n `mod` b);
int2Base :: Int -> Int -> [Char]
int2Base n b = ((int2BaseHelper n b) `mod` 10)*b^(length(int2BaseHelper n b)) ++ (int2BaseHelper (n `div` 10) b);


--convert a number string in base b1 into an Int
-- can assume input is valid
valNumString :: String -> Int -> Int
valNumString [] b = 0;
valNumString (x:xs) b1 = ((x:xs) `mod` 10)*b1^(length(xs)) + (valNumString xs b1);

--convert String of numbers in base b1 into 
-- equivalent value in base b2, as a String
-- again, all input will be valid
convert :: String -> Int -> Int -> String
convert numString b1 b2 = show(valNumString (int2Base numstring b1) b2);