-- Name: Parry, Craig
-- Net ID: parryc@unm.edu


{-
1. The function stutter takes a list of elements and 
returns a list where every element has been
duplicated. For example,

*Main> stutter "Hello World"
"HHeelllloo WWoorrlldd"
*Main> stutter [1,2,3]
[1,1,2,2,3,3]
-}
stutter [] = [] 
stutter (x:xs) = x : x : stutter xs


{-
2. The function compress eliminates consecutive
 duplicate elements of a list. For example,

*Main> compress "HHeelllloo WWoorrlldd"
"Helo World"
*Main> compress [1,2,2,3,3,3]
[1,2,3]
-}

compress (x:[]) = x:[]
compress (x:xs) = if head xs == x then compress xs else x : compress xs


{-
3. The function findIndices takes a predicate and
 a list as arguments and returns a list of numbers 
 indicating the positions of elements in the list 
 which satisfy the predicate. For example,

*Main> findIndices (< 'a') "AbCdef"
[0,2]
*Main> findIndices (== 0) [1,2,0,3,0]
[2,4]

-}
findIndices f xs = findIndices' f 0 xs
findIndices' _ _ [] = []
findIndices' f n (x:xs) = if f x then n:findIndices' f (n + 1) xs else findIndices' f (n + 1) xs


{- The function intersect takes two lists as arguments and returns a list of elements common to
both lists. For example
*Main> intersect "abc" "cat"
"ac"
*Main> intersect [1,2,3] [8]
[]
*Main> intersect [3,2,1] [1,2,3]
[3,2,1]
-}

intersect [] ys = []
--intersect (x:xs) ys = if elem x ys then x: intersect xs ys else intersect xs ys
intersect xs ys = [x | x <- xs, y <- ys, x == y]

{-
4. The function isPrefixOf takes two lists as argument and returns True iff the first list is a prefix
of the second list. For example,
*Main> "foo" `isPrefixOf` "foobar"
True
*Main> isPrefixOf [1,2,3] [4,5,6]
False
-}

isPrefixOf xs ys = foldr (&&) True (isPrefixOf' xs ys)
isPrefixOf' [] _ = [] 
isPrefixOf' xs [] = False : []
isPrefixOf' (x:xs) (y:ys) = if x == y then True : isPrefixOf' xs ys else False:[]

{- 
5. The function isSuffixOf takes two lists as argument and returns True iff the first list is a suffix
of the second list. For example,
*Main> "bar" `isSuffixOf` "foobar"
True
*Main> isSuffixOf [1,2,3] [4,5,6]
False
-}

isSuffixOf xs ys = foldr (&&) True (isSuffixOf' xs $ drop (length ys - length xs) ys)
isSuffixOf' [] _ = [] 
isSuffixOf' xs [] = False : []
isSuffixOf' (x:xs) (y:ys) = if x == y then True : isSuffixOf' xs ys else False:[]

{-
6. The dot product of two vectors ~u and ~v of length n (written ~u ·~v) is defined to be ∑
n
i=1
uivi
.
Define a function dot which takes two lists of numbers of equal length and returns their dot
product.
*Main> [0,0,1] `dot` [0,1,0]
0
-}

dot xs ys = dot' xs ys 0
dot' [] [] acc = acc
dot' (x:xs) (y:ys) acc = dot' xs ys (acc + (x*y)) 

{-
7. The function increasing takes a list of enumerable elements as its argument and returns True
if the list is sorted in increasing order and False otherwise.
*Main> increasing "ABCD"
True
*Main> increasing [100,99..1]
False
-}

increasing (x:[]) = True
increasing (x:xs) = if x < head xs then increasing xs else False

{- 
8. To ‘decimate’ literally means to kill every tenth man (it was a punishment in the Roman
legions). Define a function decimate which removes every tenth element from a list. for
example,
*Main> decimate [1..21]
[1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,21]
-}

decimate xs = [x | (x,y) <- zip xs [1..(length xs)], mod y 10 /= 0 ]

{- 
9. Define a function encipher which takes two lists of equal length and a third list. It uses
the first two lists to define a substitution cipher which it uses to encipher the third list. For
example,
*Main> encipher ['A'..'Z'] ['a'..'z'] "THIS"
"this"
-}
encipher xs ys [] = []
encipher xs ys (z:zs) =[y | (x,y) <- ciph, z == x] ++ encipher xs ys zs
  where ciph = zip xs ys 

{- 
10. Define a function prefixSum which takes a list of numbers as its argument and returns a list
of sums of all prefixes of the list. For example,
2
*Main> prefixSum [1..10]
[1,3,6,10,15,21,28,36,45,55]
*Main> prefixSum [2, 5]
[2, 7]
-}

prefixSum xs = prefixSum' xs 0
prefixSum' [] _ = []
prefixSum' (x:xs) acc = (sum) : prefixSum' xs sum where sum = x + acc

{- 
11. The function select takes a predicate and two lists as arguments and returns a list composed
of elements from the second list in those positions where the predicate, when applied to the
element in the corresponding positions of the first list, returns True.
*Main> :t select
select :: (t -> Bool) -> [t] -> [a] -> [a]
*Main> select even [1..26] "abcdefghijklmnopqrstuvwxyz"
"bdfhjlnprtvxz"
*Main> select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26]
[1,2,3,4,5,6,7]
-}
select pred xs ys = [y | (x,y) <- sel, pred x] 
  where sel = zip xs ys 

{- 
12. The function numbers which takes a list of integers as its argument and returns the integer
which has those numbers as digits. For example,
*Main> numbers [1..4]
1234
Write numbers using a tail-recursive helper function defined inside of a let expression or
using where.-}

numbers xs = numHelp xs 0 
  where numHelp [] acc = acc
        numHelp (x:xs) acc = let num = (acc * 10) + x in numHelp xs num


