-- 6.1 Basic concepts
fac :: Int -> Int
fac n = product [1..n ]

fac_r :: Int -> Int
fac_r 0 = 1 -- Base case
fac_r n = n * fac_r (n-1) -- Recursive case

(***) :: Int -> Int -> Int
m *** 0 = 0
m *** n = m + (m *** (n-1))

-- 6.2 Recursion on lists
product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
  | x <= y = x : y : ys 
  | otherwise = y : insert x ys 

-- Insertion sort using insert
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- 6.3 Multiple arguments
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs 

-- 6.4: Multiple recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- For quicksort example of multiple recursion see 1-chapter.hs

-- 6.5 Mutual recursion
even' :: Int -> Bool
even' 0 = True
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- 6.6 Advice on recursion
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs


-- Exercises
-- Exercise 1
fac_refactored:: Int -> Int
fac_refactored 0 = 1 -- Base case
fac_refactored n | n >= 0 = n * fac_refactored (n-1) -- Recursive case

-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Exercise 3
(^^^) :: Int -> Int -> Int
n ^^^ 0 = 1
n ^^^ m = n * (n ^^^ (m-1))

-- Exercise 4
euclid :: Int -> Int -> Int
euclid x y 
  | x == y = x
  | x > y = euclid (x-y) y
  | x < y = euclid x (y-x)

-- Exercise 6
-- a
and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

-- b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x!!0 : concat' xs

-- c
replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = x : replicate' (n-1) x

-- d
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

-- e
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) 
  | x == y = True
  | otherwise = elem' x ys

-- Exercise 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Exercise 8
halve :: [a] -> ([a],[a])
halve xs = (take len xs, drop len xs)
    where len = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs)))
