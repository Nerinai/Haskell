{-# language CPP #-}
{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

module Assignment3 where

#if __GLASGOW_HASKELL__ >= 804
import Prelude hiding (Monoid, mempty, foldMap, Foldable, (<>))
#elif __GLASGOW_HASKELL__ >= 710
import Prelude hiding (Monoid, mempty, foldMap, Foldable)
#endif

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List     (foldl', group, sort, sortBy, nub)
import Data.Set      (Set, empty, insert)

-- | Containers

data Rose a = a :> [Rose a]
    deriving (Eq, Show)

-- * Exercise 1

instance Functor Rose where
    fmap f (a :> as) = f a :> map (fmap f) as

class Monoid a where
    mempty ::           a
    (<>)   :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>)   = (++)


newtype Sum     a = Sum     { unSum     :: a } deriving (Eq, Show)
newtype Product a = Product { unProduct :: a } deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
    mempty           = Sum 0
    Sum n1 <> Sum n2 = Sum (n1 + n2)

-- * Exercise 2

instance Num a => Monoid (Product a) where
    mempty           = Product 1
    Product n1 <> Product n2 = Product (n1 * n2)

class Functor f => Foldable f where
    fold    :: Monoid m =>             f m -> m
    foldMap :: Monoid m => (a -> m) -> f a -> m
-- * Exercise 4
    foldMap g = fold.fmap g

instance Foldable [] where
    fold = foldr (<>) mempty

-- * Exercise 3

instance Foldable Rose where
--    fold (a :> as) = foldr (<>) a $ map fold as
    fold (a :> as) = foldr ((<>) . fold) a as
-- * Exercise 5

fsum, fproduct :: (Foldable f, Num a) => f a -> a
fsum     = unSum.foldMap Sum
fproduct = unProduct.foldMap Product

-- | Poker

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
    show  R2 = "2"
    show  R3 = "3"
    show  R4 = "4"
    show  R5 = "5"
    show  R6 = "6"
    show  R7 = "7"
    show  R8 = "8"
    show  R9 = "9"
    show  R10 = "10"
    show  J = "J"
    show  Q = "Q"
    show  K = "K"
    show  A = "A"

data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
    show (Card { rank = r, suit = s }) = show r ++ show s

type Deck = [Card]

-- * Exercise 8

fullDeck, piquetDeck :: Deck
fullDeck   = [Card r s | r <- [R2 .. A], s <- [S .. C]]
--piquetDeck = [Card r s | r <- [R7 .. A], s <- [S .. C]]
piquetDeck = [ a | a <- fullDeck, rank a >= R7]

newtype Hand = Hand { unHand :: [Card] } deriving (Eq, Show)

testhand :: Hand
testhand = Hand [Card Q S, Card R7 C, Card R7 H, Card Q S, Card Q H]

data HandCategory
    = HighCard      [Rank]
    | OnePair       Rank [Rank]
    | TwoPair       Rank Rank Rank
    | ThreeOfAKind  Rank Rank Rank
    | Straight      Rank
    | Flush         [Rank]
    | FullHouse     Rank Rank
    | FourOfAKind   Rank Rank
    | StraightFlush Rank
    deriving (Eq, Ord, Show)

-- * Exercise 9

sameSuits :: Hand -> Bool
sameSuits h = or [all ((==x).suit) $ unHand h | x <- [S ..C]]


-- * Exercise 10

isStraight :: [Rank] -> Maybe Rank
isStraight [] = Nothing
isStraight x = isStraight' $ sort x

isStraight' :: [Rank] -> Maybe Rank
isStraight' xs@(x:_:_:_:y:_)| xs == [R2,R3,R4,R5,A] = Just A
                            | xs == succList x = Just y
                            | otherwise = Nothing

succList :: Rank -> [Rank]
succList a | a >= J = []
           | otherwise = map toEnum [b .. b+4]
          where b = fromEnum a


-- * Exercise 11

ranks :: Hand -> [Rank]
ranks a = sortBy (flip compare) (map rank $ unHand a)

-- * Exercise 12

order :: Hand -> [(Int, Rank)]
order h = sortBy ordersort $ zip occurlist uniques
         where rlist = ranks h
               uniques = nub rlist
               occurlist = map (`count` rlist) uniques

ordersort :: (Int, Rank) -> (Int, Rank) -> Ordering
ordersort (x,_) (y,_) | x > y = LT
                      | x < y = GT
                      |otherwise = EQ

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count a (x:xs) | x == a = 1 + count a xs
               | otherwise = count a xs
-- * Exercise 13

handCategory :: Hand -> HandCategory
handCategory h = case isStraight $ ranks h of
                  Just x -> case sameSuits h of
                    True -> StraightFlush x
                    False -> Straight x
                  Nothing -> case order h of
                    [(4,r1),(1,r2)] -> FourOfAKind r1 r2
                    [(3,r1),(2,r2)] -> FullHouse r1 r2
                    
-- * Exercise 14

instance Ord Hand where
    compare = undefined

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs = undefined

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands = undefined

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = undefined

-- * Question 1

{- ANSWER -}

-- * Question 2

{- ANSWER -}
