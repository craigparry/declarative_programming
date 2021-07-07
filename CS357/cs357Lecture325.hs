

fact x = ifThenElse (x == 0) 1 (x * fact (x - 1))

ifThenElse x y z = if x then y else z

fib x = if x < 2 then x else fib (x - 1) + fib (x - 2)

loop x acc0 acc1 = if x == 0 then acc0 else loop (x - 1) acc1 (acc0 + acc1)

fib' x = loop x 0 1

--append xs ys = if null xs then ys else head xs : append (tail xs) ys

--length' xs = if null xs then 0 else 1 + length' (tail xs)

append' [] ys = ys 
append' (x:xs) ys = x : append' xs ys

length' [] = 0
length' (x:xs) = 1 + length' xs

[] +++ ys  = ys 
(x:xs) +++ ys = x : (xs +++ ys)

evens [] = []
evens (x:xs) = x : odds xs

odds [] = []
odds (_:xs) = evens xs

merge [] ys = ys
merge xs [] = xs 
merge us@(x:xs) vs@(y:ys) = if x < y then x:(merge xs vs) else y:(merge us ys)

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (evens xs)) (mergeSort (odds xs))

-- init "abcd" => "abc"
init' [x] = []
init' (_:xs) = last' xs

-- last "abcd" => d
last' [x] = x
last' (_:xs) = last' xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat 'z' => "zzzzzzzzz ..."
repeat' x = x : repeat' x

take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs

drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

{- replicate 4 'z' => "zzzz"
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n-1) x
-}

{- or take advantage of haskells laziness
 and use repeat take just as much as you need of hte repeated char
 -}
replicate' n x = take' n (repeat' x)

take'' (0, xs) = []
take'' (n, (x:xs)) = x : take'' ((n-1), xs)

{-
fib'' 0 acc0 _ = acc0
fib'' x acc0 acc1 = fib'' (x -1) acc1 (acc0 + acc1)
-}

{-}
fib'' (0, acc0, _) = acc0
fib'' (x, acc0, acc1) = fib'' (x -1, acc1, acc0 + acc1)
-}

funny (acc0, acc1) = (acc1, acc0 + acc1)

--iterate (named after alonso church)

church _ 0 = id
--church _ 1 = \x -> x
church f n = f . church f (n-1)

fib'' n = fst ((church funny n) (0,1))

addToList [] = []
addToList (x:xs) = (x +1) : addToList xs

sqrtAll [] = []
sqrtAll (x:xs) = (sqrt x) : sqrtAll xs

map' f [] = []
map' f (x:xs) = f x : map' f xs

--addToList = map' (+ 1) xs
-- or write 
addToList' = map' (+ 1)

sqrtAll' = map' sqrt

filter' f [] = [] 
--filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

-- or 
--filter' f (x:xs) = let rest = filter' f xs in if f x then x : rest else rest
--or 
filter' f (x:xs) = if f x then x: rest else rest 
  where rest = filter' f xs

map'' f = foldr (\x y -> f x: y) []
filter'' f xs = foldr (\x rest -> if f x then x : rest else rest) [] xs
{- or  you can remove the xs from def on both sides 
filter'' f  = foldr (\x rest -> if f x then x : rest else rest) [] -}

--length'' xs = foldr (\_ y -> 1 + y) 0 xs
length'' xs = foldr (const (+ 1)) 0 xs 

foldr' f seed [] = seed 
foldr' f seed (x:xs) = f x (foldr' f seed xs)

--e = 1/1! + 1/2! + 1/3!
--foldr (+) 0 [1,2,3,4] => 10
--sum' xs = foldr (+) 0 xs
-- or sum = foldr (+) 0
--product' = foldr (*) 1
--any' = foldr (||) False
--all' = foldr (&&) True

factn n = product[1..n]
e = sum $ map (\n -> 1/product[1..n]) [0..10]

--outerProduct (*)[1..3][1..3] => [[1,2,3],[2,4,6],[3,6,9]]
--outerProduct f xs ys = map(\x -> map(\y -> f x y) ys) xs

powerset [] = [[]]
powerset (x:xs) = half ++ (map (x :) half) where half = powerset xs



--concat [[1,2,3],[4,5,6],[7,8,9]] => [1,2,3,4,5,6,7,8,9]

--delete

--permutations [1,2,3] = permutations beginning with 1 ++ permutations beginning with 2 ++ permuations beginning with 3

permutations' [] = [[]]
permutations' xs = concat $ map (\x -> map (x:) (permutations' (filter (/= x) xs))) xs

--[x|x <-[1..10]] => [1,2,3,4,5,6,7,8,9,10]
--[x * x|x <- [1..10], even x] => [4,16,36,64,100]
-- outerProduct (*) [1,2,3] [1,2,3] => [[1,2,3],[2,4,6],[3,6,9]]
outerProduct f xs ys = [[f x y | y <- ys] | x <-xs]

--differentProduct (*) [1,2,3][1,2,3] => [1,2,3,2,4,6,3,6,9]
differentProduct f xs ys = [f x y | y<-ys, x<-xs]
--intersect {a,b,c,d} {b,c} => {b,c}
intersect xs ys = [x | x <- xs, y <- ys, x ==y]

--perfect {i,j,k} such that i*i +j*j = k*k
perfect n = [(i,j,k) | i <- [1..n], j <-[1..n], k <-[1..n], i*i + j*j == k*k]

primes n = [z | z <- [1..n], length (factors z) == 2]
  where factors n = [(x,y) | x <- [1..n], y <- [1..n], x*y == n]

permutations'' []= [[]]
permutations'' (x:xs) = [x : perms | x <- xs, perms <- permutations'' (filter (/= x) xs)]





























