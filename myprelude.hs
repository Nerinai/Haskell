module Main where

import Data.Char

main :: IO ()
main = undefined

myabs :: (Num a, Ord a) => a -> a
myabs x | x >= 0 = x
        | otherwise = -x

-- myabsJ :: Num a => a -> a
-- myabsJ

myall :: (a -> Bool) -> [a] -> Bool
myall _ [] = True
myall f x = foldr ((&&).f) True x

--recursion
myallr :: (a -> Bool) -> [a] -> Bool
myallr _ [] = True
myallr f (x:xs) = f x && myallr f xs

myallWand :: (a -> Bool) -> [a] -> Bool
myallWand f x = myand $ mymap f x

myand :: [Bool] -> Bool
myand = myfoldr (&&) True

myany :: (a -> Bool) -> [a] -> Bool
myany f = myfoldr ((||).f) False

myany' :: (a -> Bool) -> [a] -> Bool
myany' _ [] = True
myany' f xs = myfoldl (\b x -> b || f x) False xs

-- why does foldl not work

-- recursion
myanyr :: (a -> Bool) -> [a] -> Bool
myanyr _ [] = False
myanyr f (x:xs) = f x && myanyr f xs

myanyWor :: (a -> Bool) -> [a] -> Bool
myanyWor = undefined -- make when or is implemented

mybreak :: (a -> Bool) -> [a] -> ([a], [a])
mybreak = undefined -- make when span is implemented

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = myfoldl (++) x xs

--signle argument
myconcats :: [[a]] -> [a]
myconcats = myfoldr (++) []

-- recursion
myconcatr :: [[a]] -> [a]
myconcatr [] = []
myconcatr (x:xs) = x ++ myconcatr xs

mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop 0 xs = xs
mydrop n (_:xs) | n>0 = mydrop (n-1) xs
mydrop _ _ = error "negative argument"

mydropWhiler :: (a -> Bool) -> [a] -> [a]
mydropWhiler _ [] = []
mydropWhiler f a@(x:xs) | f x = mydropWhiler f xs
                        | otherwise = a

myelem :: Eq a => a -> [a] -> Bool
myelem a = myany (==a)

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs = [x | x <- xs, f x]

myflip :: (a -> b -> c) -> b -> a -> c
myflip f x y = f y x

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ a [] = a
myfoldl f a (b:bs) = myfoldl f (f a b) bs

myfoldl1 :: (a -> a -> a) -> [a] -> a
myfoldl1 _ [] = error "non empty lists in foldl1 only"
myfoldl1 f (a:as) = myfoldl f a as

myfoldr :: (a -> b-> b) -> b -> [a] -> b
myfoldr _ a [] = a
myfoldr f b (a:as) = f a (myfoldr f b as)

myfoldr1 :: (a -> a -> a) -> [a] -> a
myfoldr1 _ [] = error "non empty lists in foldr1 only"
myfoldr1 f (a:as) = myfoldr f a as

myfst :: (a,b) -> a
myfst (a,_) = a

myhead :: [a] -> a
myhead [] = error "no empty lists in myhead"
myhead (a:_) = a

myid :: a -> a
myid a = a

myinit :: [a] -> [a]
myinit [] = error "no empty list"
myinit [_] = []
myinit (x:xs) = x : myinit xs

myisalpha :: Char -> Bool
myisalpha c = isUpper c || isLower c

myisdigit :: Char -> Bool
myisdigit c = c >= '0' && c <= '9'

myislower :: Char -> Bool
myislower c = c >= 'a' && c <= 'z'

myisspace :: Char -> Bool
myisspace c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'

myisupper :: Char -> Bool
myisupper c = c >= 'A' && c <= 'Z'

myiterate :: (a -> a) -> a -> [a]
myiterate f a = a : myiterate f (f a)

mylast :: [a] -> a
mylast [] = error "no empty lists in mylast"
mylast [a] = a
mylast (_:as) = mylast as

mylenght :: [a] -> Int
mylenght [] = 0
mylenght (_:as) = 1 + mylenght as

mylenghtl :: [a] -> Int
mylenghtl xs= sum [1 | _ <- xs]

mylines :: String -> [String]
mylines = undefined -- implement after mybreak is implemented

mymap :: (a->b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

mymaplc :: (a->b) -> [a] -> [b]
mymaplc f a = [f x | x <- a]

mymax :: Ord a => a -> a -> a
mymax a b | a >= b = a
          | otherwise = b

mymaximum :: Ord a => [a] -> a
mymaximum = myfoldl1 mymax

mymin :: Ord a => a -> a -> a
mymin a b | a <= b = a
          | otherwise = b

myminimum :: Ord a => [a] -> a
myminimum = myfoldl1 mymin

mynot :: Bool -> Bool
mynot True = False
mynot False = True

myor:: [Bool] -> Bool
myor [] = True
myor (b:bs) = b || myor bs

myorh :: [Bool] -> Bool
myorh = myfoldr (||) True

myproduct :: Num a => [a] -> a
myproduct = myfoldl (*) 1

myrepeat :: a -> [a]
myrepeat a = [a | _ <- [0..]]

myreplicate :: Int -> a -> [a]
myreplicate n a = [a | _ <- [1..n]]

myreverse :: [a] -> [a]
myreverse = undefined

mysnd :: (a, b) -> b
mysnd (_,b) = b
