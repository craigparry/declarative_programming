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
data List a = Nil | Cons {car :: a, cdr :: (List a)} deriving (Show, Eq)
data Troika a b c = Troika a b c deriving (Show, Eq)

(List a) (List b) (List c) 


-- Mark 2

-- collect :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
collect Nil Nil Nil = Nil
collect (List a) (List b) (List c) = Cons (Troika  (car a) (car b) (car c) ) collect (cdr a) (cdr b) (cdr c)

-- collect' :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
--collect' = undefined

-- Mark 3

x = Cons 1 (Cons 2 Nil)
y = Cons True (Cons False Nil)
z = Cons 'a' (Cons 'b' Nil)

testCollect1 = collect x y z == Cons (Troika 1 True 'a')(Cons (Troika 2 False 'b') Nil)
--testCollect2 = collect' x y z == Cons (Troika 1 True 'a')(Cons (Troika 2 False 'b') Nil)

-- Mark 4

-- gather :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
--gather = undefined

-- Mark 5
-- Mark 6

-- translate :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
--translate = undefined

-- translate' :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
--translate' = undefined

-- Mark 7
-- Mark 8

-- convert :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
--convert = undefined

-- Mark 9
-- Mark 10

-- revert :: INSERT TYPE SIGNATURE HERE then UNCOMMENT
--revert = undefined

-- Mark 11

--simpleCheck = and [iHaveAgreedToTheAcademicHonestyPledge, testCollect1, testCollect2, testGather1, testGather2, testGather3, testTranslate1, testTranslate2, testTranslate1', testTranslate2']
