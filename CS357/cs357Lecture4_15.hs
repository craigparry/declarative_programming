--4/13

--parametric polymorphism, ex. head :: [a] -> a
--ad hoc polymorphism, ex. show :: Show a => a -> String 

-- 300 BC Euclidean algorithm
-- 8/4 => 2/1
-- 3/9 => 1/3

import Prelude hiding (zipWith)

gcd' (x, y) = 
  if x == y then x
  else if x < y then gcd' (x, y - x)
  else gcd' (x - y, x)

lcm' (x, y) = head [x * i | i <- [1..y], j <- [1..x], x * i == y * j]

--Rational == Rat
data Rat = Rat {numr :: Int, denr :: Int} 

instance Show Rat where 
  show (Rat x 1) = show x
  show (Rat 0 _) = "0"
  show (Rat x y) = show x ++ "/" ++ show y

(%) :: Int -> Int -> Rat
x % y = let z = gcd x y in Rat (x `div` z) (y `div` z)

-- a/b = c/d when a*d == b*c 
instance Eq Rat where 
  (Rat a b) == (Rat c d) = a*d == b*c 
 --x == y = numr x * denr y == numr y * denr x

instance Num Rat where 
  (Rat a b) * (Rat c d) = (a*c) % (b*d)
  (Rat a b) + (Rat c d) = (a*d + c*b) % (b*d)
  fromInteger x = fromIntegral x % 1

instance Fractional Rat where 
  (Rat a b)/ (Rat c d) = (a*d) % (b*c)

instance Ord Rat where 
  (Rat a b) > (Rat c d) = a*d > b*c
  (Rat a b) < (Rat c d) = a*d < b*c

instance Enum Rat where 
  succ (Rat a b) = if a < b then Rat (succ a ) b else Rat 1 (succ b)
  fromEnum x = assoc x $ zip rats ints 
  toEnum x = assoc x $ zip ints rats

--assoc :: x -> [(x,y)] -> y
assoc x = snd . head . dropWhile ((/= x) . fst)

ints = 1 : map succ ints
rats' = Rat 0 1 : map succ rats'
rats = filter reduced rats'

reduced (Rat a b) = a == x && b == y where (Rat x y) = a % b



-- multiply two lists with map works the same as zipWith
--map (uncurry (*)) $ zip [1..9] [1..9]
--zipWith (*) [1..9] [1..9]

data BTree a = Leaf a| Fork (BTree a) (BTree a) deriving (Show, Eq)

foo = Fork (Fork (Fork (Fork (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)
bar = Fork (Fork (Fork (Fork (Leaf 5) (Leaf 4)) (Leaf 3)) (Leaf 2)) (Leaf 1)

zipWithBTrees :: (a-> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBTrees f (Leaf x) (Leaf y) = Leaf (f x y)
zipWithBTrees f (Fork xt xt') (Fork yt yt') = Fork (zipWithBTrees f xt yt) (zipWithBTrees f xt' yt')

zipBTrees = zipWithBTrees (,)

data List a = Nil | Cons a (List a) deriving (Show, Eq)

ls0 = Cons 1 (Cons 2 (Cons 3 Nil))
ls1 = Cons 3 (Cons 2 (Cons 1 Nil))

zipWithLists :: (a->b ->c) -> List a -> List b -> List c
zipWithLists _ Nil Nil = Nil
zipWithLists f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithLists f xs ys)


data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)

xt = Node 0 [Node 1 [], Node 2 [Node 3 [], Node 4 [Node 5 []]]]
yt = Node 5 [Node 4 [], Node 3 [Node 2 [], Node 1 [Node 0 []]]]

zipWithRoseTrees :: (a->b ->c) -> RoseTree a -> RoseTree b -> RoseTree c
zipWithRoseTrees f (Node x []) (Node y []) = Node (f x y) []
zipWithRoseTrees f (Node x xs) (Node y ys) = Node (f x y) (zipWith (zipWithRoseTrees f) xs ys)

-- make zippable typeclass to make a polymorphic function 

class Zippable z where 
  zipWith :: (a -> b -> c) -> z a -> z b -> z c

instance Zippable BTree where 
  zipWith = zipWithBTrees

instance Zippable List where 
  zipWith = zipWithLists

instance Zippable [] where 
  ...

instance Ziappable RoseTree where 
  zipWith f (Node x []) (Node y []) = Node (f x y) []




--end 
