--
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ e [] = e
foldl' f e (x:xs) = foldl' f (f e x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)


-- Zadanie 39
{-
ghci> :t sum
sum :: (Foldable t, Num a) => t a -> a
ghci> :t product
product :: (Foldable t, Num a) => t a -> a
ghci> :t all
all :: Foldable t => (a -> Bool) -> t a -> Bool
ghci> :t any
any :: Foldable t => (a -> Bool) -> t a -> Bool
-}


-- Zadanie 40
{-
ghci> xs = [1..10000000]
ghci> :set +s
ghci> foldl (+) 0 xs
50000005000000
(2.34 secs, 1,612,383,576 bytes)
ghci> foldr (+) 0 xs
50000005000000
(1.97 secs, 1,615,384,952 bytes)
ghci> sum xs
50000005000000
(0.24 secs, 880,083,296 bytes)
-}


-- Zadanie 41
countodd xs = foldr (\a b -> a `mod` 2 + b) 0 xs
counteven xs = (length xs) - (countodd xs)


-- Zadanie 42


-- Zadanie 43


-- Zadanie 44


-- Zadanie 45


-- Zadanie 46


-- Zadanie 47
approx n = foldr (\a b -> (1 / a) + b) 0 [(foldl (*) 1 [1..k]) | k <- [1..n]]


-- Zadanie 48
alternatesum xs = foldl (\sum (x,one) -> (x * one) + sum) 0 (zip xs [(-1)^(k+1) | k <- [1..(length xs)]])


-- Zadanie 49
-- myfilter p = concat . map box
--     where box x = 


-- Zadanie 50
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x:xs) = if (f x) then x : (myTakeWhile f xs) else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile f (x:xs) = if (f x) then myDropWhile f xs else x:xs


-- Zadanie 51
-- Uproszczony wzór na wariancję ciągu:
-- (x1^2 + ... + xn^2)/n + ((x1 + ... + xn)/n)^2
avgvar :: Fractional a => [a] -> (a, a)
-- avgvar xs = (average, variance)
--     where average = (fst tuple) + len
--           variance = ((snd tuple) + len) ^2
--           tuple = foldl (\(a, aa) (b, bb) -> (a + b, aa + bb)) (0.0,0.0) (zip xs (map (^2) xs))
--           len = length xs
avgvar xs = (average, variance)
    where average = (fst (fst tuple)) / len
          variance = ((snd (fst tuple)) / len) ^2
          tuple = foldl (\((a, aa), l) (b, bb) -> ((a + b, aa + bb), l + 1.0)) ((0.0,0.0),0.0) (zip xs (map (^2) xs))
          len = snd tuple


-- Zadanie 52

