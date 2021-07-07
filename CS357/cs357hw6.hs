{-
Name: Parry, Craig
Net ID: parryc@unm.edu
-}

import Data.Char
import Data.List
-- Problem 1, bits2num
bits2num :: Num a => [Char] -> a
bits2num xs = fromIntegral (loop (reverse xs) 1 0)
  where loop (x:[]) n acc = ((n * (number x)) + acc)
        loop (x:xs) n acc = loop xs (n*2) ((n * (number x)) + acc)
        number x = digitToInt x 

p1tests = [bits2num "1011000" == 88]


-- Problem 2, num2bits
num2bits :: Integral a => a -> [Char]

num2bits n = '0' : (reverse $ loop n [])
  where loop 0 acc = acc
        loop n acc = loop (n `quot` 2) (acc ++ (if (n `rem` 2) == 1 then "1" else "0"))
        
p2tests = [num2bits 87783 == "010101011011100111"]


-- Problem 3, variance
variance :: (Num a, Fractional a) => [a] -> a
variance xs = foldr (+) 0 [ (x - mean) ^ 2 | x <- xs] / len
  where mean = foldr (+) 0 xs / len
        len = fromIntegral (length xs)

p3tests = [variance [1..10] == 8.25]


-- Problem 4, difference
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys= nub [x | x<-xs, not (elem x ys)]

p4tests = [difference "ABCD" "AD" == "BC",difference "ABCDCBA" "AD" == "BC"]


-- Problem 5, splits
splits ::  Ord a => [a] -> [([a], [a])]


splits (x:[]) = [([x],[])]
splits xs = temp 
  where temp = reverse [(a,b) | (a,b)<-helper (fromIntegral (length xs)) 1 xs [], (difference xs (a++b)) == [] ]
        helper n k xs ys = if n == k || n == 0 then ys else helper n (k + 1) xs (ys ++ [(a,b)| a <- combinations k xs, b <- combinations (n - k) xs])
        combinations n xs = [ ys | ys<-(combo n xs), increasing ys]
        combo 0 _ = [[]]
        combo n xs = [ x:comb | x <- xs, comb <- combo (n - 1) (filter (/= x) xs)]
        increasing xs = and $ map (uncurry (<=)) $ zip xs (tail xs)



p5tests = [sort (splits "abc") == sort ([("c","ab"),("b","ac"),("bc","a"),("a","bc"),("ac","b"),("ab","c")])]


-- Problem 6, argmin
argmin ::  (Ord a) => (t -> a) -> [t] -> t

argmin f xs = foldl1 (\x y->  if (f x <= f y) then x else y) xs 

p6tests = [argmin length ["ABC","EF","GHIJ","K"] == "K"]


data Htree a = HLeaf Double a | HFork Double [a] (Htree a) (Htree a) deriving (Show, Eq)
-- Problem 7, bogus

instance (Ord a) => Ord (Htree a) where
    (HLeaf x _) < (HLeaf y _) = x < y
    (HLeaf x _) < (HFork y _ _ _) = x < y
    (HFork x _ _ _) < (HLeaf y _) = x < y
    (HFork x _ _ _) < (HFork y _ _ _) = x < y
    (HLeaf x _) <= (HLeaf y _) = x <= y
    (HLeaf x _) <= (HFork y _ _ _) = x <= y
    (HFork x _ _ _) <= (HLeaf y _) = x <= y
    (HFork x _ _ _) <= (HFork y _ _ _) = x <= y

-- encode character using Huffman coding tree
encode (HFork _ _ (HLeaf _ l) (HLeaf _ r)) c = if c == l then "0" else "1"
encode (HFork _ _ (HLeaf _ l) v@(HFork _ rs _ _)) c =
    if c == l then "0" else '1':(encode v c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HLeaf _ r)) c =
    if c == r then "1" else '0':(encode u c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HFork _ rs _ _)) c =
    if c `elem` ls then '0':(encode u c) else '1':(encode v c)

-- decode message using Huffman coding tree
decode t [] = []
decode t (x:xs) = loop t (x:xs)
    where loop (HLeaf _ l) xs = l:(decode t xs)
          loop (HFork _ _ u v) ('0':xs) = loop u xs
          loop (HFork _ _ u v) ('1':xs) = loop v xs


bogus :: Ord a => [(Double, a)] -> Htree a
--Fork (add all elements doubles) (concat all elements characters) (create Tree with the first part of the minimum tuple should be list)
-- (create Tree with the second part of the minimum tuple should be list)
bogus xs = if (length xs) > 1 then (HFork (addFstF xs) (listElem xs) (bogus (fst $ minimum' xs)) (bogus (snd $ minimum' xs)) ) else HLeaf (fst (head xs)) (snd (head xs))
  -- minimum is creating a function that calculatesthe difference between the two parts of each tuple 
  --in the list created by splits and passes that to argmin to find the smallest occurance  which I tested with bogusTemp was working 
minimum' :: Ord b => [(Double, b)] -> ([(Double, b)], [(Double, b)])
minimum' [x] = ([x], [])
minimum' xs = argmin (\ys -> abs ((addFstF . fst) ys - (addFstF . snd) ys)) (splits xs) 
addFstF = (foldr ((+) . fst) 0) -- folds with add the first element of each tuple in the list 
listElem xs = foldr ((:) . snd) [] xs -- folds with concat the second element of each tuple in the list


bogusTemp = [(0.30,'e'), (0.14,'h'), (0.1,'l'), (0.16,'o'), (0.05,'p'), (0.23,'t'), (0.02,'w')]

p7tests = let xs = [(0.30,'e'), (0.14,'h'), (0.1,'l'), (0.16,'o'), (0.05,'p'), (0.23,'t'), (0.02,'w')] in [(decode (bogus xs) $ concatMap (encode (bogus xs)) "hello") == "hello", concatMap (encode (bogus xs)) "hello" /= concatMap (encode (bogus xs)) "oellh"]


-- Problem 8, church
church :: Int -> (a -> a) -> a -> a
church n = (\f xs -> (foldr (.) id [f | _<-[1..n]]) xs) 

p8tests = [church 4 tail "ABCDEFGH" == "EFGH", church 100 id 9001 == 9001]


data Btree a = BLeaf a | BFork (Btree a) (Btree a) deriving (Show, Eq, Ord)
-- Problem 9, trees

trees :: (Ord t) => [t] -> [Btree t]
--return leaf
trees [x]= [BLeaf x]
-- creates permuations with the list comp, using the left and right part of the splits tuple to create 
--the branch permutations and returns a list of subtrees to be combined at the top level with the list comp
trees xs = [BFork y z | x <- splits xs, y <-trees (fst x), z <- trees (snd x)]
 
p9tests = [(sort $ trees "ABCDE") !! 114 == BFork (BLeaf 'A') (BFork (BFork (BFork (BLeaf 'E') (BLeaf 'B')) (BLeaf 'C')) (BLeaf 'D')), length (trees [0..4]) == 1680]


bases = "AGCT"
-- Problem 10, insertions
insertions :: String -> [String]

insertions xs = concat [loop (length xs) c xs| c <- bases]
  where loop 0 c xs = create 0 0 c xs : []
        loop n c xs = create 0 n c xs : loop (n-1) c xs
        create ind n c [] = if ind == n then (c:[]) else []
        create ind n c q@(x:xs) = if ind == n then c: create (ind + 1) n c q else x : create (ind + 1) n c xs
 
p10tests = [sort (insertions "GC") == sort (["AGC","GAC","GCA","GGC","GGC","GCG","CGC","GCC","GCC","TGC","GTC","GCT"])]


-- Problem 11, deletions
deletions :: [a] -> [[a]]

deletions xs = reverse( loop len (zip xs [1..len]))
  where len = fromIntegral(length xs) 
        loop 0 xs = []
        loop n xs = [ a |(a,b)<-xs, b /= n ] : loop (n-1) xs

p11tests = [deletions "AGCT" == ["GCT","ACT","AGT","AGC"]]


-- Problem 12, substitutions
substitutions :: String -> [String]

substitutions xs = concat [loop ((length xs)-1) c xs| c <- bases]
  where loop 0 c xs = create 0 0 c xs : []
        loop n c xs = create 0 n c xs : loop (n-1) c xs
        create ind n c [] =  []
        create ind n c (x:xs) = if ind == n then c: create (ind + 1) n c xs else x : create (ind + 1) n c xs


p12tests = [(sort $ substitutions "ACT") == sort(["ACT","AAT","ACA","GCT","AGT","ACG","CCT","ACT","ACC","TCT","ATT","ACT"])]

-- Problem 13, transpositions
transpositions :: [a] -> [[a]]

transpositions xs = loop xs [] 1 (length xs)
  where loop xs ys n len =  if n == len then ys else loop xs (ys ++ [(create 1 n xs)]) (n+1) len
        create ind n [] = []
        create ind n (x:[]) =  x:[]
        create ind n (x1:x2:xs) = if ind == n then x2:x1: create (ind + 2) n xs else x1 : create (ind + 1) n (x2:xs)

p13tests = [transpositions "GATC" == ["AGTC","GTAC","GACT"]]


tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests,p13tests]
likelyCorrect = let results = [and t | t <- tests] in (and results, filter (not.snd) $ zip [1..] results)