curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y= f (x,y) 

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x,y) = f x y
 


test :: (Eq c) => (a -> b -> c) -> a -> b -> [Bool]
test f a b = [f a b == (curry' $ uncurry' f) a b, (uncurry f) (a,b) == (uncurry' f) (a,b)]

correct :: Bool
correct = and $ test (:) 'a' []