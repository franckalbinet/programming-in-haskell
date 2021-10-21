-- Chapter 2.5
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
    where
        b = 1
        c = 2

d = a * 2

-- Exercice 3
n = a `div` length xs
    where 
        a = 10
        xs = [1,2,3,4,5]

-- Exercise 4
last_my xs = xs !! (length xs - 1)

-- Exercise 5
init_v1 xs = reverse (tail (reverse xs))

init_v2 xs = reverse (drop 1 (reverse xs))


