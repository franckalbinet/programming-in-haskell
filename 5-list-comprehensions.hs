-- Library imports
import Data.Char
-----------------------------
-- Chapter 5.1: basic concepts
-----------------------------
concat' :: [[a]] -> [a]
concat' xxs = [x | xs <- xxs, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

----------------------
-- Chapter 5.2: Guards
----------------------
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0] 

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k'] 

--------------------------------
-- Chapter 5.3: the zip function
--------------------------------
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs] 

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]  

-------------------------------------
-- Chapter 5.4: String comprehensions
-- They are just list of characters
-------------------------------------
lowers :: String -> Int
lowers xs = length [ x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x' == x] 

---------------------------------
-- Chapter 5.5: the Caesar cipher
---------------------------------
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Letter frequency from corpus
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- `fromIntegral` converts an integer to float
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n| x <- ['a'..'z']]
           where n = lowers xs

-- Cracking the cipher
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/2 | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

------------
-- Exercises
------------
-- Exercise 1
--[x^2 | x <- [1..100]]

-- Exercise 2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Exercise 3
square :: Int -> [(Int, Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y] 

-- Exercise 4
replicate' :: Int -> a -> [a]
replicate' n a = [a | x <- [0..n]]

-- Exercise 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], z^2 == x^2 + y^2]

-- Exercise 6you 
pop :: [a] -> [a]
pop xs = reverse (tail (reverse xs))

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum (pop (factors x)) == x]

-- Exercise 7
ex_7 :: [(Int,Int)]
ex_7 = [(x,y) | x <- [1,2], y <- [3,4]]
-- solution missing

-- Exercise 8
positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0..])  

-- Exercise 9: scalar product
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

