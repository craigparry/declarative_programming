import Data.List

--prolem 1 -- write flip 

flip' :: (a-> b->c) -> (b -> a -> c)

flip' f = \x y -> f y x

-- (define flip (lambda (f) (lambda (x y) ...

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f seed [] = seed
foldr' f seed (x:xs) = f x (foldr' f seed xs)

-- problem 2 -- write filter using foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr' (\x y -> if f x then x : y else y) []
-- or filter' f xs = foldr' (\x y -> if f x then x : y else y) [] xs


-- problem 3 - write set differene as infix operatro (\\) as a list comprehension


xs \\\ ys  =[x | x <- xs, not (elem x ys)]

--parametric polymorphism
--ad hoc polymorphism (type classes)

-- problem 4 -- write powerset 
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = half ++ map (x :) half
  where half = powerset xs


  -- be familiar with flip, compose, foldr, prodcedural abstraction stuff, currying 


-- a -> b -> c
-- 1. (a -> (b -> c))
--2. (a -> b) -> c
--3. (a -> b -> c)




--zip3' xs ys zs = [(x,y,z)| x <-xs, y<-ys, z<-zs]
zip3' [] [] [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z): zip3' xs ys zs

testZip3 = (zip3' "01" [False, True] [0,1])

unzip3' xs = help xs [] [] []
  where help ((x,y,z):[]) as bs cs = (as ++ [x], bs ++[y], cs++[z])
        help ((x,y,z):xs) as bs cs = help xs (as++ [x]) (bs++[y]) (cs++[z])

nub' xs = help xs []
  where help [] ys = ys 
        help (x:xs) ys = if elem x ys then help xs ys else help xs (ys ++ [x])


