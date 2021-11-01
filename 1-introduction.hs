-- Quick sort sorting algorithm
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
                

-- Exercise 2
product' [] = 1
product' (n:ns) = n * product' ns

-- Exercise 4
qsort_r [] = []
qsort_r (x:xs) = qsort_r larger ++ [x] ++ qsort_r smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
 
-- Exercise 5
qsort_w [] = []
qsort_w (x:xs) = qsort_w smaller ++ [x] ++ qsort_w larger
    where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]
 
