import Prelude hiding (lookup, (>>=))
--import Data.list hiding (lookup)

--true unbiased fair beautiful pretty beguiling artful artificial sham false

--type String = [Char]

thesaurus :: [(String, [String])]
thesaurus = [("true",["accurate", "unbiased", "authentic", "factual", "genuine"]),
             ("unbiased",["candid","evenhanded", "fair", "impartial", "just"]),
             ("fair", ["beautiful", "comely", "pretty"]),
             ("beautiful", ["gorgeous","pretty","lovely", "ravishing"]),
             ("pretty", ["beguiling","beautiful", "comely", "fair", "lovely"]),
             ("beguiling", ["artful","cute","devious","deceitful"]),
             ("artful",["artificial","crafty","cunning","guileful","subtle"]),
             ("artificial", ["cunning","guileful","sham","synthetic"]),
             ("sham", ["bogus","ersatz","false","synthetic"])]


lookup :: Eq a => [(a,[b])] -> a ->[b]
lookup [] word = []
lookup ((word' , synonyms): rest) word = if word == word' then synonyms else lookup rest word

--lookup thesaurus (lookup thesaurus (lookup thesaurus word))

xs >>= f = concat $ map f xs 
{-
chain1 word = lookup thesarus word 
chain2 word = lookup thesaurus word >>= lookup thesaurus 
chain3 word = lookup thesaurus word >>= lookup thesaurus >>= lookup thesaurus 

-}


f >=> g = (>>= g) . f

chain1 = lookup thesaurus 
chain2 = chain1 >=> chain1
chain3 = chain1 >=> chain1 >=> chain1

chain 1 = lookup thesaurus 
chain n = chain (n - 1) >=> lookup thesaurus


data RoseTree a = Node a [RoseTree a] deriving (Show, Eq)
-- infinite tree 
--synonymTree word = Node word [synonymTree synonym | synonym <- lookup thesaurus word]

synonymTree path@(word:_) = Node path [synonymTree (synonym  : path)| synonym <- lookup thesaurus word]



data Queue a = Nil | Queue {front :: [a], back :: [a]} deriving (Show, Eq)

-- 0(1) enqueue 
enqueue y Nil = Queue [] [y]
enqueue y (Queue xs ys) = Queue xs (y:ys)

-- 0(1) amortized enqueue
dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [x] []) = (x, Nil)
dequeue (Queue [] [x]) = (x, Nil)
dequeue (Queue [] ys) = (x, Queue xs []) where (x: xs) = reverse ys
dequeue (Queue (x:xs) ys) = (x, Queue xs ys)

q = enqueue 3 (enqueue 4 ( enqueue 5 Nil))
{-
bfs' xt = loop (xt, Nil) []
  where loop (Empty, Nil) acc1 = reverse acc1
        loop (Leaf x, Nil) acc1 = reverse (x: acc1)
        loop (Empty, acc0) acc1 = loop (dequeue acc0) acc1
        loop (Leaf x, acc0) acc1 = loop (dequeue acc0) (x:acc1)
        loop (Fork x yt zt, acc0) acc1 = loop (dequeue (enqueue zt ( enqueue yt acc0))) (x:acc1)
-}
enqueueList [] q = q
enqueueList (x:xs) q = enqueue x ( enqueueList xs q)

bfs (Node x xs, q) = x : bfs (dequeue (enqueueList xs q))


--if it exists then we can find a chain from word to word'
	-- if it doesnt exist it will infinitely look inside the infinite roseTree
findChain word word' = reverse . head $ dropWhile ((/= word') . head) (bfs (synonymTree [word], Nil))


