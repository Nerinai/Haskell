module Main where

import Data.Char

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

main :: IO ()
main = undefined

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, isAsciiLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n = map $ shift n

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a'..'z']]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop m xs ++ take m xs
              where m = n `mod` length xs

crack :: String -> String
crack xs = encode (-factor) xs
            where
              factor = head (positions (minimum chitab) chitab)
              chitab = [chisqr (rotate n table') table | n <- [0..25]]
              table' = freqs xs

-- gird :: Int -> Int -> [(Int,Int)]
-- grid = undefined
-- grid x y = [(a,b) | a <- [0..x], b <- [0..y]]
--
square :: Int -> [(Int,Int)]
square x = [(a,b)| a <- [0..x], b <- [0..x], a /= b]

myReplicate :: Int -> a -> [a]
myReplicate n thing = [thing | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]
