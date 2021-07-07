{-
Name: Parry, Craig
Net ID: parryc@unm.edu
-}

-- Note: you cannot put a = b, where b is some built in function. I.e. no myTakeWhile = takeWhile
-- Problem 1, myTakeWhile
myTakeWhile :: (a-> Bool ) -> [a] -> [a]
myTakeWhile pred xs = [x | (x , y) <- ziplist, y < value] 
  where value = head([y | (x , y) <- ziplist, not (pred x)])
        ziplist = zip xs [1..]

p1tests = [myTakeWhile (/= ' ') "This is practice." == "This"]


-- Problem 2, mySpan
mySpan :: (a->Bool) -> [a] -> ([a],[a])
mySpan pred xs = ([x | (x , y) <- ziplist, y < value] , [x | (x , y) <- ziplist, y >= value])
  where value = head([y | (x , y) <- ziplist, not (pred x)])
        ziplist = zip xs [1..]

p2tests = [mySpan (/= ' ') "This is practice." == ("This"," is practice.")]


-- Problem 3, combinations3
combinations3 :: Ord a => [a] -> [[a]]
combinations3 xs = [a:b:c:[] | a<-xs, b<-xs, c<-xs, a < b && b < c]

p3tests = [combinations3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]


-- Problem 4, runLengthEncode
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode (x:xs) = pairs xs (x , 1)
  where pairs [] tup@(y , n) = [tup]
        pairs (x:xs) tup@(y,n) = if x == y then pairs xs (y, (n + 1)) else tup : runLengthEncode (x:xs)

p4tests = [runLengthEncode [4,2,2,1,1,1,1,4,4,4,4] == [(4,1),(2,2),(1,4),(4,4)], runLengthEncode "foo" == [('f',1),('o',2)]]


-- Problem 5, runLengthDecode
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode xs = foldr (++) [] [loop x y | (x,y) <- xs]
  where loop x 0 = []
        loop x y = x : loop x (y - 1) 

p5tests = [runLengthDecode [(4,1),(2,2),(1,4),(4,4)] == [4,2,2,1,1,1,1,4,4,4,4], (runLengthDecode $ runLengthEncode "foobar") == "foobar"]


-- Problem 6, splitText
splitText :: Ord a => (a -> Bool) -> [a] -> [[a]]

splitText pred xs = help ([] , []) pred xs
  where help (ys, zs) pred [] = if null ys then zs else zs ++ (ys:[])
        help (ys, zs) pred (x:xs) = if pred x 
                                    then help (ys++ (x:[]),zs) pred xs
                                    else help ([], zs ++ (ys:[])) pred xs

p6tests = [splitText (/= ' ') "This is practice." == ["This","is","practice."]]


-- Problem 7, encipher
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher xs ys [] = []
encipher xs ys (z:zs) =[y | (x,y) <- ciph, z == x] ++ encipher xs ys zs
  where ciph = zip xs ys 

p7tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this",encipher [1..10] (map (\x -> x*x) [1..10]) [10,9..1] == [100,81,64,49,36,25,16,9,4,1],encipher [10,9..0] [10,9..0] [0..10] == [0,1,2,3,4,5,6,7,8,9,10],encipher (['A','C'..'Z'] ++ ['B','D'..'Z']) [1..26] ['A'..'Z'] == [1,14,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,22,10,23,11,24,12,25,13,26]]


-- Problem 8, goldbach
goldbach :: Int -> [(Int, Int)]
goldbach n = [(a,b) | a <-[2..n], b<- [2..n], a+b == n && prime a && prime b && a <= b]
  where prime num = foldr (&&) True [mod num x /= 0 |x<-[2,3..(num - 1)]]

p8tests = [goldbach 6 == [(3,3)], goldbach 9 == [(2,7)]]


-- Problem 9, increasing
increasing :: Ord a => [a] -> Bool
increasing xs = and $ map (uncurry (<=)) $ zip xs (tail xs)

p9tests = [increasing "ABBD", not $ increasing [100,99..1]]


-- Problem 10, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select pred xs ys = [y | (x,y) <- sel, pred x] 
  where sel = zip xs ys 

p10tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 11, combinations
combinations :: Ord a => Int -> [a] -> [[a]]

combinations n xs = [ ys | ys<-(combo n xs), increasing ys ]
  where combo 0 _ = [[]]
        combo n xs = [ x:comb | x <- xs, comb <- combo (n - 1) (filter (/= x) xs)]

p11tests = [combinations 3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]

-- Note: Uncomment the pNtests and in tests below and in tests once you have given a definiton for problem 12

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


tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests] ++[p12tests,p13tests,p14tests,p15tests]
likelyCorrect = (and [and t | t <- tests], if length tests < 15 then "lacking ComplexInteger tests?" else "")