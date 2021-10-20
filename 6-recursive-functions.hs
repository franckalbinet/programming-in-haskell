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
product_my :: Num a => [a] -> a
product_my [] = 1
product_my (n:ns) = n * product_my ns

length_my :: [a] -> Int
length_my [] = 0
length_my (_:xs) = 1 + length_my xs

reverse_my :: [a] -> [a]
reverse_my [] = []
reverse_my (x:xs) = reverse_my xs ++ [x] 

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
zip_my :: [a] -> [b] -> [(a,b)]
zip_my [] _ = []
zip_my _ [] = []
zip_my (x:xs) (y:ys) = (x,y) : zip_my xs ys 

drop_my :: Int -> [a] -> [a]
drop_my 0 xs = xs
drop_my _ [] = []
drop_my n (x:xs) = drop_my (n-1) xs 

-- 6.4: Multiple recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- For quicksort example of multiple recursion see 1-chapter.hs

-- 6.5 Mutual recursion
even_my :: Int -> Bool
even_my 0 = True
even_my n = odd (n-1)

odd_my :: Int -> Bool
odd_my 0 = False
odd_my n = even (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- 6.6 Advice on recursion
init_my :: [a] -> [a]
init_my [_] = []
init_my (x:xs) = x : init_my xs


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
and_my :: [Bool] -> Bool
and_my [x] = x
and_my (x:xs)
  | x == False = False
  | otherwise = and_my xs

-- b
concat_my :: [[a]] -> [a]
concat_my [] = []
concat_my (x:xs) = x!!0 : concat_my xs

-- c
replicate_my :: Int -> a -> [a]
replicate_my 1 x = [x]
replicate_my n x = x : replicate_my (n-1) x

-- d
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)


