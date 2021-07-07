val :: ([[Char]], (([[Char]], [[Char]]), Char))
val = ([['t']], (([['e']], [['s']]), 't'))

answer :: ([[Char]], (([[Char]], [[Char]]), Char))
answer = ((('t':[]):[]) , (( (('e':[]):[]) ,(('s':[]):[])) , 't'))
    -- using only the following functions and values:
        -- () :: ()
        -- (,) :: a -> b -> (a, b)
        -- (:) :: a -> [a] -> [a]
        -- 't'
        -- 'e'
        -- 's'
    --      no answer = ([['t']], (([['e']], [['s']]), 't'))
    -- you may accuse the TA of reusing problems
    -- you may use a where to define individual parts of your answer

participation :: Bool
participation = (==) val answer