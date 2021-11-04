-- Chapter 4.3 - Guarded equations
abs' n 
  | n >= 0    = n
  | otherwise = -n

-- Chapter 4.4 - Pattern matching 

-- Exercices
-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = splitAt idx xs
    where idx = (length xs) `div` 2


-- Exercise 2
third_a :: [a] -> a
third_a xs = head (tail (tail xs))

third_b :: [a] -> a
third_b xs = xs!!2

third_c :: [a] -> a
third_c (x1:(x2:(x3:xs))) = x3


-- Exercise 3
safetail_a :: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs   

safetail_b :: [a] -> [a]
safetail_b xs 
  | null xs = []
  | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c []  = []
safetail_c xs = tail xs


-- Exercise 4
(|||) :: Bool -> Bool -> Bool
False ||| False = False
False ||| True  = True
True  ||| False = True
True  ||| True  = True

(||||) :: Bool -> Bool -> Bool
False |||| False = False
_     |||| _     = True

(|||||) :: Bool -> Bool -> Bool
False ||||| b    = b
True  ||||| _    = True

(||||||) :: Bool -> Bool -> Bool
a |||||| b 
  | a == b    = a
  | otherwise = True


-- Exercise 5
(&&&) :: Bool -> Bool -> Bool
(&&&) a b = if a == True then
                         if b == True then True 
                                      else False
                         else False


-- Exercise 6
(&&&&) :: Bool -> Bool -> Bool
(&&&&) a b = if a == True then b else False

-- Exercise 7
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z


mult_v2 :: Int -> (Int -> (Int -> Int))
mult_v2 = \x -> (\y -> (\z -> x*y*z))


-- Exercise 8 (Luhn algorithm)
-- Naïve implementation. 
-- Chapter 7 on higher-order function will
-- provide alternative and more elegant implementation
luhnDouble :: Int -> Int
luhnDouble n 
  | 2*n < 9 = 2*n
  | otherwise = 2*n - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =  (mod sum 10) == 0 
    where sum = luhnDouble a + b + luhnDouble c + d
