{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

--import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = a :> [Rose a]
    deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (x :> _) = x

children :: Rose a -> [Rose a]
children ( _ :> x ) = x

-- Exercise 2

size :: Rose a -> Int
size (_ :> []) = 1
size (_ :> xs) = 1 + sum (map size xs)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> xs) = sum (map leaves xs)
-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((a1,a2,a3),
           (b1,b2,b3),
           (c1,c2,c3)) = ((a3,b3,c3),
                          (a2,b2,c2),
                          (a1,b1,c1))

diagonals :: Board -> (Row, Row)
diagonals ((a1,_,a3),
            (_,b2,_),
           (c1,_,c3)) = ((a1, b2, c3),
                           (a3, b2, c1))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B,B,B),
              (B,B,B),
              (B,B,B))

testBoard :: Board
testBoard = ((B,B,B),
             (B,X,B),
             (B,B,O))

testWinnerVert :: Board
testWinnerVert = ((B,X,B),
                  (B,X,B),
                  (B,X,B))

testWinnerHori :: Board
testWinnerHori = ((X,X,X),
                  (B,B,B),
                  (B,B,B))

testWinnerDiag :: Board
testWinnerDiag = ((X,B,B),
                  (B,X,B),
                  (B,B,X))

testWinnerO :: Board
testWinnerO = ((O,O,O),
               (B,B,B),
               (B,B,B))

testWinnerDraw :: Board
testWinnerDraw = ((X,O,X),
                  (O,O,X),
                  (X,X,O))

testWinnerOneMove :: Board
testWinnerOneMove = ((X,O,X),
                     (O,O,X),
                     (X,B,X))

testBrokenBoard :: Board
testBrokenBoard = ((O,X,X),
                   (O,B,X),
                   (O,X,O))

board2List :: Board -> [[Field]]
board2List ((a,b,c),
            (d,e,f),
            (g,h,i)) = [[a,b,c],
                        [d,e,f],
                        [g,h,i]]

diagonalList :: (Row, Row) -> [[Field]]
diagonalList ((a,b,c),(d,e,f)) = [[a,b,c],[d,e,f]]

list2Board :: [[Field]] -> Board
list2Board [[a,b,c],
            [d,e,f],
            [g,h,i]] = ((a,b,c),
                        (d,e,f),
                        (g,h,i))
list2Board _ = error "wrong board"

board2FlatList :: Board -> [Field]
board2FlatList x = concat $ board2List x

flatList2Board :: [Field] -> Board
flatList2Board [a,b,c,d,e,f,g,h,i] = list2Board [[a,b,c],[d,e,f],[g,h,i]]
flatList2Board _ = error "wrong board"

-- Exercise 7

printBoard :: Board -> String
printBoard x = intercalate "\n-+-+-\n" (map (intercalate "|".map show) boardlist) ++ "\n"
              where boardlist = board2List x

-- | Move generation

-- Exercise 8

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n a (x:xs) | n == 0 = a : xs
                   | otherwise = x : replace (n-1) a xs

moves :: Player -> Board -> [Board]
moves player b = map (flatList2Board . (\x -> replace x (symbol player) flatlist)) permutelist
                  where flatlist = board2FlatList b
                        permutelist = elemIndices B flatlist

-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner x | any (all (== X)) allrows = Just P1
            | any (all (== O)) allrows = Just P2
            | otherwise = Nothing
              where allrows = board2List x ++ board2List (verticals x) ++ diagonalList  (diagonals x)


isWinner :: Board -> Bool
isWinner x = case winres of
              Just _ -> True
              Nothing -> False
            where winres = hasWinner x

-- Exercise 10
gameTree :: Player -> Board -> Rose Board
gameTree p b | [] == moves p b || isWinner b = b :> []
             | otherwise = b :> map (gameTree (nextPlayer p)) (moves p b)

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves $ gameTree P1 emptyBoard

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax p t@(_ :> _) = minimax' p p t

minimax' :: Player -> Player -> Rose Board -> Rose Int
minimax' p _ (b :> []) = case hasWinner b of
                          Just x              -> resolve p x
                          Nothing             ->  0 :> []
                        where resolve z y | z == y    = 1 :> []
                                          | otherwise = (-1) :> []

minimax' p t (_ :> bs) | p == t    = maximum' ns :> bs'
                       | otherwise = minimum' ns :> bs'
                        where bs'  = map (minimax' p (nextPlayer t)) bs
                              ns   = [x | x :> _ <- bs']


-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' (-1 : _) = -1
minimum' (x : y : xs) | x < y = minimum' $ x : xs
                      | otherwise = minimum' $ y : xs
minimum' [x] = x

maximum' :: [Int] -> Int
maximum' (1 : _) = 1
maximum' (x : y : xs) | x > y = maximum' $ x : xs
                      | otherwise = maximum' $ y : xs
maximum' [x] = x


-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove p b | isWinner b = Nothing
             | otherwise = case zet of
                  [] -> Nothing
                  _  -> Just bestmove
              where zet = moves p b
                    zipmoves = zip zet ts
                    best :> ts = minimax p (gameTree p b)
                    (bestmove : _) = [x | (x , z :> _ ) <- zipmoves, z == best]

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"
main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just z  -> putStrLn (show z ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do putStrLn "Possible moves are:"
                                  putStrLn (listMoves possibleMoves)
                                  i <- askFor "Make your choice:" [1 .. length possibleMoves]
                                  return (Just (possibleMoves !! (i - 1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map (lines
                      . (\ (i, b) -> "(" ++ show i ++ "): \n" ++ printBoard b))
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y
