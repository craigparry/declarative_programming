-- 4/3
--Record Type Human prefix First Last Age | Dog Name Age

--data Entity = Human String String String Int | Dog String Int deriving (Show)

data Entity = Human {title :: String, firstName :: String, lastName :: String, age :: Int} | Dog {firstName :: String, age :: Int} deriving (Show)

bill = Human "Mr." "William" "Baggins" 32
rover = Dog "Rover" 7

--greet x = "Hello " ++ firstName x

--greet x@(Human _ _ _ _) = "Hello " ++ title x ++ firstName x 
--greet x@(Dog _ _)       = "Hello " ++ firstName x

greet (Human title firstName lastName _) = "Hello " ++ title ++ firstName 
greet (Dog name _)       = "Hello " ++ name

data Coin = Penny | Nickel | Dime | Quarter | Half | Dollar deriving (Show, Eq)

value Penny = 1
value Nickel = 5
value Dime = 10
value Quarter = 25 
value Half = 50 
valut Dollar = 100

-- point free definition (no free variables)
total = sum . map value

powerset [] = [[]]
powerset (x:xs) = half ++ map (x : ) half where half = powerset xs

--makeChange amount coins = head $ filter (\x -> total x == amount) (powerset coins)
--makeChange amount coins = head $ filter ((== amount) . total) (powerset coins)
makeChange amount = head . (filter ((== amount) . total)) . powerset

foo = [Penny, Penny, Penny, Dime, Dime, Nickel, Half, Dime, Penny, Quarter, Penny]

data List a = Nil | Cons {car :: a, cdr :: List a} deriving (Show, Eq)

bar = Cons 'a' (Cons 'b' (Cons 'c' Nil))

--append xs ys = if xs == Nil then ys else Cons (car xs) (append (cdr xs) ys)

append Nil ys = ys 
append (Cons x xs) ys = Cons x (append xs ys)

reverse' Nil = Nil
reverse' (Cons x xs) = append (reverse' xs) (Cons x Nil)

haskell2scheme :: [a] -> List a

haskell2scheme [] = Nil
haskell2scheme (x:xs) = Cons x (haskell2scheme xs)

scheme2haskell :: List a -> [a]
scheme2haskell Nil = []
scheme2haskell (Cons x xs) = x : (scheme2haskell xs)

data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving(Show, Eq)

tree = Fork (Fork (Fork (Fork (Leaf 0) (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)

size :: BTree a -> Int
size (Leaf _) = 1 
size (Fork xt yt) = size xt + size yt

height :: BTree a -> Int
height (Leaf _) = 0 
height (Fork xt yt) = 1 + height xt `max` height yt

flatten :: BTree a -> [a]
flatten (Leaf x) = [x]
flatten (Fork xt yt) = flatten xt ++ flatten yt

makeBTree :: [a] -> BTree a 
makeBTree [x] = Leaf x
makeBTree xs = makeBTree (take m xs) `Fork` makeBTree (drop m xs)
  where m = length xs `div` 2 

mapBTree :: (a -> b) -> BTree a -> BTree b 
mapBTree f (Leaf x) = Leaf (f x)
mapBTree f (Fork xt yt) = mapBTree f xt `Fork` mapBTree f yt

-- flat-recur (fold) => map
-- deep-recur (deep-recur) => deep-reverse, deep-flatten
foldBTree :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldBTree f g (Leaf x) = f x
foldBTree f g (Fork xt yt) = g (foldBTree f g xt) (foldBTree f g yt)

size' = foldBTree (const 1) (+)
--height' = foldBTree (const 0) (\x y -> 1 + x `max` y)
height' = foldBTree (const 0) ((+ 1) .: max)

-- function that takes two curried args and composes two functions on them example
--((+1) .: max) 3 4 => 5
f .: g = \x y -> f (g x y)

--flatten' = foldBTree (\x -> [x]) (++)
--point free
flatten' = foldBTree (: []) (++)

--mapBTree' f = foldBTree (\x -> Leaf (f x)) Fork

mapBTree' f = foldBTree (Leaf . f) Fork

data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)

x = Node 5 [Node 1 [], Node 2 [Node 3 [], Node 4 [Node 5 []]]]

fringe (Node x []) = [x]
--fringe (Node _ xs) = foldr (++) [] (map fringe xs) 
--fringe (Node _ xs) = concat (map fringe xs) 
fringe (Node _ xs) = concatMap fringe xs

flattenRoseTree :: RoseTree a -> [a]
flattenRoseTree (Node x []) = [x]
flattenRoseTree (Node x xs) = x : concatMap flattenRoseTree xs 

sumRoseTree :: Num a => RoseTree a -> a
sumRoseTree (Node x []) = x
sumRoseTree (Node x xs) = x + (sum $ map sumRoseTree xs)







