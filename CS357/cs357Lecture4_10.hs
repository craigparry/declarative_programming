
data Tree a = Empty | Leaf a | Fork a (Tree a) (Tree a) deriving (Show, Eq)
--      j
--     / \
--    f   k
--   /\   /\
--  a  h    z
--  \
--    d 

foo = Fork 'j' (Fork 'f' (Fork 'a' Empty (Leaf 'd')) (Leaf 'h')) (Fork 'k' Empty (Leaf 'z'))

--jfadhkz
dfs :: Tree a -> [a]
dfs Empty = []
dfs (Leaf x) = [x]
dfs (Fork x yt zt) = x : (dfs yt ++ dfs zt)

--  LIFO
dfs' :: Tree a -> [a]
dfs' xt = loop [xt] []
    where loop [] acc1 = reverse acc1 
          loop (Empty : acc0) acc1 = loop acc0 acc1
          loop ((Leaf x): acc0) acc1 = loop acc0 (x:acc1)
          loop ((Fork x yt zt) : acc0 ) acc1 = loop (yt : zt : acc0) (x:acc1)


--[j]      []
--[f k]    [j]
--[a h k]  [f j]
--[d h k]  [a f j]
--[h k].   [d a f j]
--[k].     [h d a f j]
--[z].     [h k d a f j]
--[].      [z k h d a f j]

--  FIFO
bfs :: Tree a -> [a]
bfs xt = loop [xt] []
    where loop [] acc1 = reverse acc1 
          loop (Empty : acc0) acc1 = loop acc0 acc1
          loop ((Leaf x): acc0) acc1 = loop acc0 (x:acc1)
          loop ((Fork x yt zt) : acc0 ) acc1 = loop (acc0 ++ [yt] ++ [zt]) (x:acc1)

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

bfs' xt = loop (xt, Nil) []
  where loop (Empty, Nil) acc1 = reverse acc1
        loop (Leaf x, Nil) acc1 = reverse (x: acc1)
        loop (Empty, acc0) acc1 = loop (dequeue acc0) acc1
        loop (Leaf x, acc0) acc1 = loop (dequeue acc0) (x:acc1)
        loop (Fork x yt zt, acc0) acc1 = loop (dequeue (enqueue zt ( enqueue yt acc0))) (x:acc1)

enqueueList [] q = q
enqueueList (x:xs) q = enqueue x ( enqueueList xs q)

bfs (Node x xs, q) = x : bfs (dequeue (enqueueList xs q))