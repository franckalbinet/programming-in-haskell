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
          deriving Show

-- Examples of logical propositions
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Var substitution rules
-- `Assoc` type is defined above as a lookup table
type Subst = Assoc Char Bool

-- Example
substRuleExample:: Subst
substRuleExample = [('A',False),('B',True)]

-- Evaluation
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- Returns all variables in a proposition
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     =  [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- Produces list of logical values
type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools' :: Int -> [[Bool]]
bools' n = map (reverse . map conv . make n . int2bin) range 
    where 
        range     = [0..(2^n)-1]
        make n bs = take n (bs ++ repeat 0) 
        conv 0    = False
        conv 1    = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n-1)


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 8.7 Abstract machine (extended example)
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

-- Below eval is determined by Haskell
-- Howeve such control information can be made explicit
-- creating our own "Abstract Machine"

-- Control stack
type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value' :: Expr -> Int
value' e = eval' e []

-- Exercises
-- Exercise 1
mult :: Nat -> Nat -> Nat
mult Zero n        = Zero
mult (Succ m) n    = add n (mult m n) 

-- Exercise 2
-- Requires only one comparison (previous version may requires two)
occursAlt :: Ord a => a -> Tree a -> Bool
occursAlt x (Leaf y)     = x == y
occursAlt x (Node l y r) = case (compare x y) of
                             LT -> occursAlt x l
                             EQ -> True
                             GT -> occursAlt x r


-- Exercise 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
    deriving Show

-- Example of balanded tree
tBalanced :: Tree' Int
tBalanced = Node'
                (Node'
                     (Leaf' 1) (Leaf' 2)) 
                (Leaf' 3)

tUnbalanced :: Tree' Int
tUnbalanced = Node'
                  (Node' 
                       (Node' 
                            (Leaf' 1) (Leaf' 2))
                       (Leaf' 3)) 
                  (Leaf' 4)

leaves :: Tree' a -> Int
leaves (Leaf' _)   = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 
                       && balanced l && balanced r 

-- Exercise 4
halve :: [a] -> ([a],[a]) 
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance left) (balance right)
    where (left,right) = halve xs

-- Exercise 5
data Expr' = Val' Int | Add' Expr' Expr'
    deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr' -> a
folde f _ (Val' n) = f n
folde f g (Add' x y) = g (folde f g x) (folde f g y)

-- Test 
f :: Int -> Int
f n = n

g :: Int -> Int -> Int
g n m = n + m

expr = Add' (Add' (Val' 1) (Val' 2)) (Val' 3)

-- Exercise 6
-- For instance
evalEx :: Expr' -> Int
evalEx = folde (\x -> x) (\x y -> x + y)

size :: Expr' -> Int
size = folde (\x -> 1) (\x y -> x + y)

-- Exercise 7
data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Just' a == Just' b   = a == b

--instance Eq a => Eq [a] where
--   [] == []         = True
--    (x:xs) == (y:ys) = (x == y) && (xs) == (ys)

-- Exercise 8 and 9 (TBD)



