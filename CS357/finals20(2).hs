-- CS 357: Declarative Programming
-- Final (Spring '20)

{- ========== Academic Honesty Pledge ========== --
I promise that I will not use an internet search engine to assist me in completing this exam. (This includes Hoogle)
I promise that I will not disclose the content of this exam to others.
I promise that I will not work collaboratively on this exam with others.
I promise that the answers I submit to the exam questions are my own and solely the product of my individual effort.
I understand that I am permitted to use a Haskell interpreter or compiler to complete this exam.
I understand that I am permitted to use the online CS 357 class course notes, my previously submitted homework, and the course's textbook to complete this exam.
I understand that violation of any of these promises will result in a formal university inquiry for academic dishonesty.
-- ============================================= -}

lastName = "Parry"
firstName = "Craig"
netID = "parryc@unm.edu" -- @unm.edu
unmIDNumber = "101510121"

iHaveAgreedToTheAcademicHonestyPledge = True

{- =========== INSTRUCTIONS =========== --
Fill out the above with accurate information.
Fill out this file with your solutions and turn it in.
The file you turn in must compile without warnings,
    if necessary leave functions undefined with partial solutions commented out.
-- ==================================== -}

{- ========== UNARY NUMERALS ========== --
Feel free to paste the information from LEARN here

The unary numeral system is the simplest numeral system able to represent natural numbers. 
In order to represent a number n, an arbitrarily chosen symbol representing 1 is repeated n times.
For example, in this system the number 0 would be represented as the empty string []
and the numbers 1, 2, 3, 4, 5, 6... would be represented as:
1, 11, 111, 1111, 11111, 111111...
 
• Define a data type called Unary which can be used to represent numbers in the unary numeral system.
• Define a function called toUnary which converts a non-negative Haskell Int to its Unary representation.
• Define a function called fromUnary which converts a Unary numeral to a Haskell Int.
• Make Unary an instance of the Haskell type classes Show.
    Note: Your Show instance should cause Haskell to display unary numerals using decimal notation, 
    i.e., the unary numeral 11 should be displayed by Haskell as the decimal number 2.
• Make Unary an instance of the Haskell type classes Eq.
• Make Unary an instance of the Haskell type classes Enum.
    Note: Your Enum instance should include toEnum, fromEnum, succ and pred.
• Without using Haskell's (+) or (--) operators, define a function plus which computes the sum of two numbers represented using Unary numerals.
• Without using Haskell's (+), (--), (*) or ( / ) operators, define a function times which computes the product of two numbers represented using Unary numerals.
• Make Unary an instance of the the Haskell type class Num.
    Note: Your Num instance should include (+) and (*).
-- ==================================== -}

-- data Unary 
data Unary = Nada | Nums {num :: Int, ls :: Unary}

toUnary :: Int -> Unary
toUnary 0 = Nada
toUnary n = Nums n (toUnary (n-1))

fromUnary :: Unary -> Int
fromUnary Nada = 0
fromUnary (Nums num ls) = num

-- instance Show 
instance Show Unary where 
  show Nada = show 0
  show (Nums num ls) = show num

-- instance Eq 
instance Eq Unary where 
  Nada == Nada = True
  Nada == (Nums num ls) = False
  (Nums num ls) == Nada = False
  (Nums a b) == (Nums c d) = a == c 
 
-- instance Enum 
instance Enum Unary where 
  succ Nada = (Nums 1 Nada)
  succ q@(Nums num ls) = (Nums (num + 1) q)
  fromEnum Nada = fromUnary Nada
  fromEnum q@(Nums num ls) = fromUnary q
  toEnum x = toUnary x

--using church for plus and times   
church :: Int -> (a -> a) -> a -> a
church n = (\f xs -> (foldr (.) id [f | _<-[1..n]]) xs) 

plus :: Unary -> Unary -> Unary
plus Nada Nada = toUnary 0
plus Nada q@(Nums num ls) = q
plus q@(Nums num ls) Nada = q
plus (Nums num1 ls1) q@(Nums num2 ls2) = church num1 succ q

times :: Unary -> Unary -> Unary
times Nada Nada = Nada
times Nada q@(Nums num ls) = Nada
times q@(Nums num ls) Nada = Nada
times (Nums num1 ls1) q@(Nums num2 ls2) = foldr plus Nada (replicate num1 q) 

-- instance Num 
instance Num Unary where 
  (Nums a b) * (Nums c d) = toUnary (a*c)
  (Nums a b) + (Nums c d) = toUnary (a + c)
  

{- ========== CALL ME FUNCTOR ========== --
The type List is defined as follows: -}
data List a = Nil | Cons {car :: a, cdr :: (List a)}
{-
• Make List an instance of the Haskell typeclass Functor.
-- ===================================== -}

-- instance Functor
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{- ============ TREE OF WOE ============ --
Consider the following data type: -}
data Tree = Leaf Int | Fork {label :: Char, left :: Tree, right :: Tree}
foo = (Fork 'j' (Fork 'f' (Fork 'a' (Leaf 1) (Leaf 1)) (Leaf 0)) (Fork 'k' (Leaf 1) (Leaf 0)))
bar = (Fork 'j' (Fork 'f' (Fork 'a' (Leaf 1) (Leaf 1)) (Leaf 1)) (Fork 'k' (Leaf 1) (Leaf 1)))
{-
Feel free to paste the information from LEARN here

• Define a function woe which takes a Tree and returns a list of labels associated with all Forks where both left and right subtrees are non-zero Leafs.
--foo ->    j              bar ->    j
--            / \                          / \
--           /   \                        /   \
--          f     k                     f     k
--         / \   / \                    / \   / \
--        a  0   1  0                  a  1   1  1
--       / \                         / \
--      1   1                       1   1
 
*Main> :t woe
woe :: Tree -> [Char]
*Main> woe foo
"a"
*Main> woe bar
"ak"
Note: The order of the characters in the returned list is not important.
-- ===================================== -}

woe :: Tree -> [Char]
woe (Fork label (Leaf a) (Leaf b)) = if (a == b) && (a /= 0) then [label] else []
woe (Fork label q@(Fork a b c) (Leaf d)) = woe q 
woe (Fork label (Leaf d) q@(Fork a b c)) = woe q 
woe (Fork label q@(Fork a b c) z@(Fork d e f)) = woe q ++ woe z

{- ============== FOLD ME ============== --
Feel free to paste the information from LEARN here
-- ===================================== -}

g :: Int -> Int
g n = foldr (*) 1 (take n (repeat n))

f :: Int -> Int
f n = foldr ((+) . g) 0 [1..n]  

