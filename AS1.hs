module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Control.Arrow

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

-- main :: IO ()
-- main = interact (lines >>> exercise >>> unlines)
--
-- exercise :: [String] -> [String]
-- exercise = parseTable >>> select "gender" "male"
--                       >>> project ["last", "first", "salary"] >>> printTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2

printLine :: [Int] -> String
printLine = foldr ((++) . ("+" ++) . (`replicate` '-')) "+"


-- * Exercise 3

printField :: Int -> String -> String
printField ln str | all isDigit str = pad ++ str
                  | otherwise = str ++ pad
                    where pad = (ln - length str)`replicate` ' '
-- * Exercise 4

printRow :: [(Int, String)] -> String
printRow x = "|"  ++ intercalate "|" (map(uncurry printField) x) ++ "|"

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths = map (maximum.map length).transpose

-- * Exercise 6

--printTable :: Table -> [String]
printTable table@(header:rows)
    = [line] ++ printrows [upheader] ++ [line] ++ printrows rows ++ [line]
      where printrows = map (printRow . zip cw)
            upheader = map (map toUpper) header
            cw = columnWidths table
            line = printLine cw

--headerCase :: Table -> Table
-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    = undefined

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    = undefined
