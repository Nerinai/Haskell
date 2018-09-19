module Main where

main :: IO ()
main = undefined
--
-- type Pos = (Int,Int)
-- type Trans = Pos -> Pos
-- type Pair a = (a,a)
--
-- data Move = North | South | East | West
--
-- move :: Move -> Pos -> Pos
-- move North (x,y) = (x,y+1)
-- move South (x,y) = (x,y-1)
-- move East  (x,y) = (x+1,y)
-- move West  (x,y) = (x-1,y)
--
-- moves :: [Move] -> Pos -> Pos
-- moves [] p = p
-- moves ms p = foldl (flip move) p ms
--
-- rev :: Move -> Move
-- rev North = South
-- rev South = North
-- rev East = West
-- rev West = East
--
-- data Nat = Zero | Succ Nat
--
-- nat2int :: Nat -> Int
-- nat2int Zero = 0
-- nat2int (Succ n) = 1 + nat2int n
--
-- int2nat :: Int -> Nat
-- int2nat 0 = Zero
-- int2nat n = Succ (int2nat (n-1))
--
-- add :: Nat -> Nat -> Nat
-- --add n m = int2nat $ nat2int n + nat2int m
-- add Zero n = n
-- add (Succ m) n = Succ $ add m n

data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
          (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
