
--referential transparency 
import System.Random

-- StdGen

-- randomRs

flips g = randomRs (0 :: Int, 1) g  

generators g0 = g1 : generators g2 where (g1,g2) = split g0

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Show, Eq, Enum, Ord)

data Rank = Two | Three | Four |Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum, Ord)

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq)

instance Ord Card where 
    (Card x _) < (Card y _) = x < y
    (Card x _) >= (Card y _) = x >= y

instance Show Card where 
    show (Card x y) = show x ++ show y

deck = [Card rank suit | rank <- [(Two)..(Ace)], suit <- [(Diamonds)..(Clubs)]]

-- riffle cut 

riffle [] [] _ = []
riffle [] ys _ = ys 
riffle xs [] _ = xs 
riffle (x:xs) (y:ys) (0:flips) = x : y : riffle xs ys flips 
riffle (x:xs) (y:ys) (1:flips) = y : x : riffle xs ys flips 

riffleCut g xs = riffle (take 26 xs) (drop 26 xs) (flips g)

shuffle g = foldr1 (.) $ take 7 $ map riffleCut (generators g)

sort [] = [] 
sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs)

straight hand = and [x /= Ace && y == succ x | (Card x _, Card y _) <- hand `zip` tail hand]

hands seed = [sort $ take 5 $ shuffle g deck | g <- generators (mkStdGen seed)]

odds seed = length $ filter straight (take 100000 (hands seed))



