-- 8.1 Type declarations (Alias or synonym declaration)
type String = [Char]

type Pos = (Int,Int)

type Trans = Pos -> Pos

-- Recursivity such as type Tree = (Int,[Tree]) not allowed

-- Can be parametrized
type Pair a = (a,a)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k' == k]

-- 8.2 Data declarations

-- data Bool = False | True

data Move = North | South | East | West 
    deriving Show    

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y)  = (x+1,y)
move West (x,y)  = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

-- Parametrized constructors
data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- Parametrized data declarations
-- In prelude:
-- data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- 8.3 Newtype declarations
-- newtype Nat = N Int

-- 8.4 Recursive types
data Nat = Zero | Succ Nat 
    deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
-- or using recursion
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ (add m n)

data List a = Nil | Cons a (List a)
    deriving Show

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs
