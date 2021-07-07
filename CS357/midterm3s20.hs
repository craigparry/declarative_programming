-- CS 357: Declarative Programming
-- Midterm 3 (Spring '20)


lastName = "Parry"
firstName = "Craig"
netID = "parryc"

iHaveAgreedToTheAcademicHonestyPledge = True

-- -----------------------------------------
-- This exam is a .hs file that must be turned in.
-- You must fill in the above fields.
-- All functions, including helper functions using where or let syntax, MUST have explicit type signatures.
-- -----------------------------------------

-- Mark 1

--COME TOGETHER 
-- The type List is defined as follows:
data List a = Nil | Cons {car :: a, cdr :: (List a)} deriving (Show, Eq)
-- The type Troika is defined as follows:
data Troika a b c = Troika a b c deriving (Show, Eq)

-- The function collect takes three Lists as arguments and returns a List of Troikas.
-- Mark 2
-- Mark 3
-- It can be used as follows:
collect :: List a -> List b -> List c -> List (Troika a b c)

collect Nil Nil Nil = Nil
collect (Cons x xs) (Cons y ys) (Cons z zs) = Cons (Troika  x y z) (collect xs ys zs)

collect' :: List a -> List b -> List c -> List (Troika a b c)

collect' a@(Cons x xs) b@(Cons y ys) c@(Cons z zs) = loop (zip3 (gather (:) [] a) (gather (:) [] b) (gather (:) [] c))
  where loop :: [(a, b, c)] -> List (Troika a b c)
        loop [] = Nil 
        loop ((a,b,c):xs) = Cons (Troika a b c) (loop xs)

--collect'' a@(Cons x xs) b@(Cons y ys) c@(Cons z zs) = gather (Cons . (\a b c -> (Troika a b c))) Nil a b c

x = Cons 1 (Cons 2 Nil)
y = Cons True (Cons False Nil)
z = Cons 'a' (Cons 'b' Nil)

testCollect1 = collect x y z == Cons (Troika 1 True 'a')(Cons (Troika 2 False 'b') Nil)
testCollect2 = collect' x y z == Cons (Troika 1 True 'a')(Cons (Troika 2 False 'b') Nil)

-- Mark 4

gather :: (a -> b -> b)-> b -> List a -> b


gather f seed Nil = seed 
gather f seed (Cons x xs) = f x (gather f seed xs)

-- Mark 5


testGather1 = gather (++) "" (Cons "code" (Cons " is" (Cons " love" Nil))) == "code is love"
testGather2 = gather Cons Nil (Cons "code" (Cons "is" (Cons "life" Nil))) == (Cons "code" (Cons "is" (Cons "life" Nil)))
testGather3 = gather (.) id (Cons succ (Cons succ Nil)) 0 == 2


-- TOMORROW NEVER KNOWS



-- The function translate translates a List to a Haskell list.

-- Mark 6

translate :: List a -> [a]
translate Nil = []
translate (Cons x xs)= x : translate xs

translate' :: List a -> [a]
translate' q@(Cons x xs)= gather (:) [] q 

-- Mark 7
testTranslate1 = translate (Cons 'g' (Cons 'o' (Cons 'o' Nil))) == "goo"

testTranslate2 = translate (Cons 0 (Cons 1 Nil)) == [0,1]

testTranslate1' = translate' (Cons 'g' (Cons 'o' (Cons 'o' Nil))) == "goo"

testTranslate2' = translate' (Cons 0 (Cons 1 Nil)) == [0,1]

-- A DAY IN THE LIFE



-- The function convert converts a function of a Troika to 
--a curried Haskell function of three arguments. It has type signature:
-- Mark 8

convert :: ((Troika a b c) -> d) -> a -> b -> c -> d


-- Mark 9

convert f = (\a b c -> f (Troika a b c)) 

-- Mark 10

revert :: Troika a b c -> (a, b, c)
revert q@(Troika a b c) =  convert (\(Troika x y z )-> (x,y,z)) a b c

-- Mark 11

simpleCheck = and [iHaveAgreedToTheAcademicHonestyPledge, testCollect1, testCollect2, testGather1, testGather2, testGather3, testTranslate1, testTranslate2, testTranslate1', testTranslate2']
