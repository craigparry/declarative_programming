
import Prelude hiding ((>>=))
--data Maybe a = Nothing | Just a

car [] = Nothing 
car (x:xs) = Just x

cdr [] = Nothing
cdr (x:xs) = Just xs
{-
-- cadddr
cadddr xs = case cdr xs of 
  Nothing -> Nothing 
  Just xs -> case cdr xs of 
    Nothing -> Nothing
    Just xs -> case cdr xs of 
      Nothing -> Nothing
      Just xs -> car xs 
      -}

eToS = [("dog", "perro"), ("cat","gato"), ("red","rioja"),("word", "palabra"),("good", "bueno"), ("head", "cabeza")]
sToG = [("perro","Hund"),("gato","Kats"),("rioja", "rot"),("palabra","Wort"), ("bueno","gut"),("cabeza","Kopf")]
gToF = [("Hund","chien"),("Katz","chat"),("rot","rouge"),("Wort","mot"),("gut","bon"),("Kopf","tete")]
fToI = [("chien","cane"), ("chat","gotto"), ("rouge","rosso"),("mot","parola"),("bon","buon"),("tete","testa")]

findKey :: Eq a =>  a -> [(a,b)] -> Maybe b
findKey key [] = Nothing 
findKey key ((x,y):xs) = if key == x then Just y else findKey key xs

translate = flip findKey
{-
englishToItalian word = 
  case translate eToS word of
    Nothing -> Nothing
    Just palabra -> case translate sToG palabra of 
      Nothing -> Nothing
      Just wort -> case translate gToF wort of 
        Nothing -> Nothing
        Just mot -> translate fToI mot
-}

-- bind (funciton in the prelude) applies functions to values
(>>=) :: Maybe t -> (t -> Maybe a) -> Maybe a 
Nothing >>= f = Nothing 
Just x >>= f = f x

--englishToItalian word = translate eToS word >>= translate sToG >>= translate gToF >>= translate fToI

--fish
--f >=> g = \x -> f x >>= g
f >=> g = (>>= g) . f

--englishToItalian word = translate eToS word >=> translate sToG >=> translate gToF >=> translate fToI


cadddr = cdr >=> cdr >=> cdr >=> car




--end