{-
Name: Parry, Craig
Net ID: parryc@unm.edu
-}

-- Problem 1, stutter
stutter :: [a] -> [a]
stutter [] = [] 
stutter (x:xs) = x : x : stutter xs

problem1tests = [stutter "Hello World" == "HHeelllloo  WWoorrlldd", stutter [1,2,3] == [1,1,2,2,3,3]]


-- Problem 2, compress
compress :: Eq a => [a] -> [a]
compress (x:[]) = x:[]
compress (x:xs) = if head xs == x then compress xs else x : compress xs

problem2tests = [compress "HHeelllloo WWoorrlldd" == "Helo World",  compress [1,2,2,3,3,3] == [1,2,3]]


-- Problem 3, findIndices
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices f xs = findIndices' f 0 xs
findIndices' _ _ [] = []
findIndices' f n (x:xs) = if f x then n:findIndices' f (n + 1) xs else findIndices' f (n + 1) xs

problem3tests = [findIndices (< 'a') "AbCdef" == [0,2],findIndices (== 0) [1,2,0,3,0] == [2,4]]


-- Problem 3.5, intersect
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] ys = []
intersect xs ys = [x | x <- xs, y <- ys, x == y]

problem35tests = [intersect "abc" "cat" == "ac", intersect [1,2,3] [8] == [], intersect [3,2,1] [1,2,3] == [3,2,1]]


-- Problem 4, isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf xs ys = foldr (&&) True (isPrefixOf' xs ys)
isPrefixOf' [] _ = [] 
isPrefixOf' xs [] = False : []
isPrefixOf' (x:xs) (y:ys) = if x == y then True : isPrefixOf' xs ys else False:[]

problem4tests = ["foo" `isPrefixOf` "foobar", not $ isPrefixOf [1,2,3] [4,5,6]]


-- Problem 5, isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = foldr (&&) True (isSuffixOf' xs $ drop (length ys - length xs) ys)
isSuffixOf' [] _ = [] 
isSuffixOf' xs [] = False : []
isSuffixOf' (x:xs) (y:ys) = if x == y then True : isSuffixOf' xs ys else False:[]

problem5tests = ["bar" `isSuffixOf` "foobar", not $ isSuffixOf [1,2,3] [4,5,6]]


-- Problem 6, dot
dot :: [Int] -> [Int] -> Int
dot xs ys = dot' xs ys 0
dot' [] [] acc = acc
dot' (x:xs) (y:ys) acc = dot' xs ys (acc + (x*y)) 

problem6tests = [[0,0,1] `dot` [0,1,0] == 0]


-- Problem 7, increasing
increasing :: (Ord a) => [a] -> Bool
increasing (x:[]) = True
increasing (x:xs) = if x < head xs then increasing xs else False

problem7tests = [increasing "ABCD", not $ increasing [100,99..1]]


-- Problem 8, decimate
decimate :: [a] -> [a]
decimate xs = [x | (x,y) <- zip xs [1..(length xs)], mod y 10 /= 0 ]

problem8tests = [decimate [1..21] == [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,21]]


-- Problem 9, encipher
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher xs ys [] = []
encipher xs ys (z:zs) =[y | (x,y) <- ciph, z == x] ++ encipher xs ys zs
  where ciph = zip xs ys 

problem9tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this"]


-- Problem 10, prefixSum
prefixSum :: (Num a) => [a] -> [a]
prefixSum xs = prefixSum' xs 0
prefixSum' [] _ = []
prefixSum' (x:xs) acc = (sum) : prefixSum' xs sum where sum = x + acc

problem10tests = [prefixSum [1..10] == [1,3,6,10,15,21,28,36,45,55], prefixSum [2, 5] == [2, 7]]


-- Problem 11, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select pred xs ys = [y | (x,y) <- sel, pred x] 
  where sel = zip xs ys 

problem11tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 12, numbers
numbers :: [Int] -> Int
numbers xs = numHelp xs 0 
  where numHelp [] acc = acc
        numHelp (x:xs) acc = let num = (acc * 10) + x in numHelp xs num

problem12tests = [ numbers [1..4] == 1234]


tests = [problem1tests,problem2tests,problem3tests,problem35tests,problem4tests,problem5tests,problem6tests,problem7tests,problem8tests,problem9tests,problem10tests,problem11tests,problem12tests]
likelyCorrect = and $ map and tests


