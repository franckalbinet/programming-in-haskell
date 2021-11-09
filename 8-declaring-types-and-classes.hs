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

-- Binary tree
data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

-- Example of tree
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

-- May traverse the entire tree (in the worst case)
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- If applying flatten to a tree gives a sorted list it is called a "Search tree"
-- In such case: occurs could be refactored:
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) 
  | x == y         = True
  | x < y          = occurs x l
  | otherwise      = occurs x r

-- Note that trees in computing come in many different forms:
-- data only on their leaves
-- data Tree a = Leaf a | Node (Tree a) (Tree b)

-- data only in their nodes
-- data Tree a = Leaf | Node (Tree a) a (Tree a)

-- data of different types in ther leaves and nodes
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)

-- have a list of subtrees
-- data Tree a = Node a [Tree a]

-- 8.5 Class and instance declarations
-- Recall that:
--   - a `type` is a collection of values and that 
--   - a `class` is a collection of types that support certain overloaded operations called methods

-- 8.6 Tautology checker (extended example)
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

