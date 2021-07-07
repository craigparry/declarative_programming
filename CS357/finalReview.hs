--Final review

-- Num, Ord, Enum, Show, Eq, Functor
-- need to know how to define and instance them
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

-- Problem 12, ComplexInteger, real, imaginary
data ComplexInteger a = ComplexInteger {real :: a, imaginary :: a}

p12tests = [real (ComplexInteger 1 2) == 1, imaginary (ComplexInteger 2 3) == 3]

-- Problem 13, Eq
instance Eq a => Eq (ComplexInteger a) where

  (ComplexInteger a b) == (ComplexInteger c d) = a == c && b == d
p13tests = [(ComplexInteger 1 2) /= (ComplexInteger 3 4)]


-- Problem 14, Show
instance (Show a, Ord a, Num a) =>  Show (ComplexInteger a) where
 
  show (ComplexInteger 0 1) = "1i"
  show (ComplexInteger 0 n) = show n ++"i"
  show (ComplexInteger n 0) = show n
  show (ComplexInteger a b) = if b > 0 then show a ++ "+" ++ show b ++ "i" else show a ++ show b ++ "i"
  
p14tests = [(show $ ComplexInteger 1 2) == "1+2i", (show $ ComplexInteger 1 0) == "1", (show $ ComplexInteger 0 1) == "1i"]

-- Problem 15, Num
instance Num a => Num (ComplexInteger a) where

  (ComplexInteger a b) + (ComplexInteger c d) = (ComplexInteger (a+b) (c+d))
  (ComplexInteger a b) * (ComplexInteger c d) =  (ComplexInteger ( ( a * c) - (b * d) ) ((a * d) + (b * c)) )
p15tests = [(ComplexInteger 1 2) * (ComplexInteger 3 4) == (ComplexInteger (-5) 10)]

-- Functor (fmap) polymorphic version of map that can be used on data structures that are not lists

import Prelude hiding (Functor,fmap, zip, zipWith, zip3, map)

class Functor m where 
  fmap :: (a -> b) -> m a -> m b

instance Functor [] where
  fmap f [] = []
  fmap f (x:xs) = f x : fmap f xs


data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving (Show, Eq)

foo = Fork (Leaf 1) (Leaf 2)

instance Functor BTree where 
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Fork xt yt) = Fork (fmap f xt) (fmap f yt)


--Exam 3 

data List a = Nil | Cons {car :: a, cdr :: List a} deriving (Show, Eq)

data Troika a b c = Troika a b c deriving (Show, Eq)


--Zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
collect :: List a -> List b -> List c -> List (Troika a b c)
collect Nil _ _ = Nil
collect _ Nil _ = Nil
collect _ _ Nil = Nil
collect (Cons x xs) (Cons y ys) (Cons z zs) = Cons (Troika x y z) (collect xs ys zs)

collect' 
-- foldr :: (a->b->b)->b -> [a] ->b

gather :: (a->b->b)->b->List a->b
gather g seed Nil = seed
gather g seed (Cons x xs) = g x (gather g seed xs)

translate :: List a -> [a]
translate Nil = []
translate (Cons x xs) = x :translate xs

translate' = gather (:) []

bar = Cons 'b' (Cons 'a' (Cons 'r' Nil))

collect'' xs ys zs = foldr Cons Nil $ zipWith3 Troika (translate' xs) (translate' ys) (translate' zs)


--curry3 :: (a,b,c) -> d -> a ->b -> c -> d
convert f = \a-> \b-> \c -> f (Troika a b c)

revert :: Troika a b c -> (a,b,c)
revert = convert (,,)

zip3 xs ys zs = zipWith (\(x,y)-> \z -> (x,y,z)) (zip xs ys) zs

zipWith f xs ys = map (uncurry f) $ zip xs ys 

map f = foldr (\x ys-> f x : ys) []

--zip [] = const []
--zip (x:xs) = \(y:ys) -> (x,y) : zip' xs ys

--zip (x:xs) = \(y:ys) -> (x,y): cont ys where cont is zip xs 
step:: x -> [y] -> [(x,y)]-> [y] -> [(x,y)]
step x cont = \(y:ys) -> (x,y): cont ys 

--type synonym
--type String = [Char]

type Cont x y = [y]-> [(x,y)]

zip = foldr step (const [])








--end