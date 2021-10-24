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

-- The foldr function
-- Foldr versions of sum, product, or, and
sum_fold :: Num a => [a] -> a
sum_fold = foldr (+) 0

product_fold :: Num a => [a] -> a
product_fold = foldr (*) 1

or_fold :: [Bool] -> Bool
or_fold = foldr (||) False

and_fold :: [Bool] -> Bool
and_fold = foldr (&&) True

fold_r_my :: (a -> b -> b) -> b -> [a] -> b
fold_r_my f v [] = v
fold_r_my f v (x:xs) = f x (fold_r_my f v xs)

length_my :: [a] -> Int
length_my = foldr (\_ n -> 1+n) 0 

reverse_my :: [a] -> [a]
reverse_my = foldr (\x xs -> xs ++ [x]) []
