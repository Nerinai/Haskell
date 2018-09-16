chrmodule Main where

main :: IO ()
main = interact reverse

sumUpTo :: (Num n, Eq n) => n -> n
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n-1)

sumUpTo' :: (Enum n, Num n) => n -> n
sumUpTo' n = sum [0..n]

qsort :: Ord x => [x] -> [x]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
                where
                  smaller = [a | a <- xs, a <= x]
                  bigger  = [a | a <- xs, a > x]

qsortreverse :: Ord x => [x] -> [x]
qsortreverse [] = []
qsortreverse (x:xs) = qsortreverse smaller ++ [x] ++ qsortreverse bigger
                where
                  smaller = [a | a <- xs, a > x]
                  bigger  = [a | a <- xs, a <= x]

seqn :: Monad m => [m t] -> m [t]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)


my_product :: Num a => [a] -> a
my_product [] = 1
my_product (a:as) = a * my_product as

greetname :: [Char] -> [Char] -> [Char]
greetname firstName lastName = "Hello, " ++ firstName ++ " " ++ lastName


mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake _ [] = []
mytake x (y:ys) = y : take (x-1) ys

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || elem x ys

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (y:y2:ys) = y <= y2 && sorted y2:ys
