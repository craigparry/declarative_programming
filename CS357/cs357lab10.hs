increasing :: Ord a => [a] -> Bool

increasing xs = and $ map (uncurry (<)) $ zip xs (tail xs)

-- define localMaxima using a list comprehension and zip3
--  such that, given a list of orderable items
--              returns a list of the items that are greater than
--              their predecesor and their successor
localMaxima :: Ord a => [a] -> [a]
localMaxima xs = [ b |(a,b,c) <- zip3 xs (tail xs) (tail (tail xs)), b > a && b > c]

tests = [localMaxima [1..10] == [], localMaxima [1,2,1,7,8,3,9,9,10] == [2,8]]