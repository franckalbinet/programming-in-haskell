-- Imports
import Data.Char
import Data.List

-- 7.1 Basic concepts
add :: Int -> Int -> Int
add x y = x + y

-- actually means
add_l :: Int -> (Int -> Int)
add_l = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2 Processing lists
map_my :: (a -> b) -> [a] -> [b]
map_my f xs = [f x | x <- xs]

map_r :: (a -> b) -> [a] -> [b]
map_r f [] = []
map_r f (x:xs) = f x : map_r f xs

filter_my :: (a -> Bool) -> [a] -> [a]
filter_my p xs = [x | x <- xs, p x]

filter_r :: (a -> Bool) -> [a] -> [a]
filter_r p [] = []
filter_r p (x:xs)  
  | p x = x : filter_r p xs
  | otherwise = filter_r p xs

-- 7.3 The foldr function
-- Foldr versions of sum, product, or, and
sum_foldr :: Num a => [a] -> a
sum_foldr = foldr (+) 0

product_fold :: Num a => [a] -> a
product_fold = foldr (*) 1

or_fold :: [Bool] -> Bool
or_fold = foldr (||) False

and_fold :: [Bool] -> Bool
and_fold = foldr (&&) True

foldr_my :: (a -> b -> b) -> b -> [a] -> b
foldr_my f v [] = v
foldr_my f v (x:xs) = f x (foldr_my f v xs)

length_my :: [a] -> Int
length_my = foldr (\_ n -> 1+n) 0 

reverse_my :: [a] -> [a]
reverse_my = foldr (\x xs -> xs ++ [x]) []

-- 7.4 The foldl function
sum_my :: Num a => [a] -> a
sum_my = sum' 0
    where 
        sum' v [] = v
        sum' v (x:xs) = sum' (v+x) xs

-- Functions previously defined using foldr can be redefined. 
-- Ex. of sum:
sum_foldl :: Num a => [a] -> a
sum_foldl = foldl (+) 0

foldl_my :: (a -> b -> a) -> a -> [b] -> a
foldl_my f v [] = v
foldl_my f v (x:xs)  = foldl f (f v x) xs

-- 7.5 The composition operator

-- 7.6 Binary transmitter extended example
type Bit = Int

-- Naîve implementation
bin2int_naive :: [Bit] -> Int
bin2int_naive bits = sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1

-- Using foldr
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7.7 Voting algorithms
-- First past the post
votes :: [String]
votes = ["Red","Blue","Green","Blue","Blue","Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [((count v vs), v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote
ballots :: [[String]]
ballots = [["Red","Green"],
          ["Blue"],
          ["Green","Blue"],
          ["Blue","Green"],
          ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c] -> c
               (c:cs) -> winner' (elim c bs)

-- Exercises
-- Exercise 1
ex1 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
ex1 f p xs = [f x | x <- xs, p x]

ex1' :: (a -> a) -> (a -> Bool) -> [a] -> [a]
ex1' f p xs = (map f . filter p) xs

-- Exercice 2
all' :: (a -> Bool) -> [a] -> Bool
-- Using foldr
--all' p xs = (foldr (&&) True) (map p xs)
-- Using composition operator
all' p  = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) 
  | p x = x : takeWhile' p xs
  | otherwise = takeWhile' p []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) 
  | p x = dropWhile' p xs
  | otherwise = x : xs

-- Exercise 3
map' :: (a -> a) -> [a] -> [a]
map' f = foldr (\x xs -> f x:xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- Exercise 4
-- Test [2,3,4,5] -> 2345
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> 10*acc + x) 0

-- Exercice 5
-- Implement curry and uncurry functions
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- Exercise 6
unfold :: (t1 -> Bool) -> (t1 -> t) -> (t1 -> t1) -> t1 -> [t]
unfold p h t x 
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map_unfold :: (a -> b) -> [a] -> [b]
map_unfold f = unfold null (f . head) (tail) 

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f



