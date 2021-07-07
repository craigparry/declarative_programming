{-Huffman Coding 

sort source alphabet in decreasing order of probability 
they are the leaves of the tree 
merge r source symbols with smallest probability into a new 
source symbol with probability equal to the sum of the two leaves  
-}

-- sort can be found in the data list library 
import Data.List

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

xs = [(Leaf 0.305119 'e'), (Leaf 0.13995 'h'),(Leaf 0.098061 'l'),(Leaf 0.160739 'o'),(Leaf 0.050271 'p'),(Leaf 0.227369 'e'),(Leaf 0.305119 'e'),(Leaf 0.305119 'e')]



-- use splits for homework problem 
--splits [a,b,c,d] = [([a,b],[c,d]),([a],[b,c,d]),...

--use splits and merge to find the best split at each step 
-- use argmin to find the smallest value at each step 

square difference amount the sets agrmin 


