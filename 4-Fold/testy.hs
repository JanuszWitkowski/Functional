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
nondec xs = foldl (\t (a, b) -> t && a <= b) True (zip xs (tail xs))


-- Zadanie 43


-- Zadanie 44 TODO
-- 1st draft
ssm' [] = []
ssm' [x] = [x]
ssm' (x:xs) = fst (
    foldl (\(path, rest) next -> 
        if (last path) >= next then (path, tail rest) 
        else (path ++ (maxPath (ssm' (tail rest)) (ssm' (next:(tail rest)))), tail rest)
        ) ([x], xs) xs )
        where maxPath ys zs = if (length ys) > (length zs) then ys else zs
-- The real deal
-- ssm :: Num a => [a] -> [a]
-- ssm [] = []
-- ssm [x] = [x]
-- ssm (x:xs) = let clearList y = filter (> y)
--                  incrementPath :: (Num a) => [[a]] -> [a] -> a -> [a] -> [a]
--                  incrementPath [] [] y maxPath = maxPath ++ [y]
--                  incrementPath ps sys y maxPath = if (last sys) < y && (length maxPath) < (length $ last ps) then incrementPath (init ps) (init sys) y (last ps) else incrementPath (init ps) (init sys) y maxPath
--                  longestPath maxP [] = maxP
--                  longestPath maxP ys = if (length maxP) < (length $ head ys) then (longestPath (head ys) (tail ys)) else (longestPath maxP (tail ys))
--              in longestPath [] $ fst $ foldl (\(paths, sublist) e -> (paths ++ (incrementPath paths sublist e []), sublist ++ [e])) ([[x]], [x]) (clearList x xs)
            --  in longestPath [] $ fst $ foldl (\(paths, sublist) e -> (paths ++ [[x]], sublist ++ [e])) ([[x]], [x]) (clearList x xs)
-- xs = [3,2,1,5,3,2,6,2,3,8]
-- ys = [3,1,69,4,5,6,7,8,9,0,77,1]
--- TEST ---
clearList'' y = filter (> y)
-- incrementPath'' :: (Num a) => [[a]] -> [a] -> a -> [a] -> [a]
-- incrementPath'' [] [] y maxPath = maxPath ++ [y]
-- incrementPath'' ps sys y maxPath = if (last sys) < y && (length maxPath) < (length $ last ps) then incrementPath'' (init ps) (init sys) y (last ps) else incrementPath'' (init ps) (init sys) y maxPath
-- longestPath'' maxP [] = maxP
-- longestPath'' maxP ys = if (length maxP) < (length $ head ys) then (longestPath'' (head ys) (tail ys)) else (longestPath'' maxP (tail ys))
-- ssm'' :: Num a => [a] -> [a]
-- ssm'' [] = []
-- ssm'' [x] = [x]
-- ssm'' (x:xs) = longestPath'' [] $ fst $ foldl (\(paths, sublist) e -> (paths ++ (incrementPath'' paths sublist e []), sublist ++ [e])) ([[x]], [x]) (clearList'' x xs)


-- Zadanie 45
remdupl [] = []
remdupl (x:xs) = foldl (\zs y -> if y /= (last zs) then zs ++ [y] else zs) [x] xs


-- Zadanie 46
-- LMAO czyt. Zadanie 41


-- Zadanie 47
approx n = foldr (\a b -> (1 / a) + b) 0 [(foldl (*) 1 [1..k]) | k <- [1..n]]
approx' n = fst $ foldl (\(x, y) a -> (x + (y/a), y/a)) (0, 1) [1..n]


-- Zadanie 48
alternatesum xs = foldl (\sum (x,one) -> (x * one) + sum) 0 (zip xs [(-1)^(k+1) | k <- [1..(length xs)]])


-- Zadanie 49
myfilter p = concat . map box
    where box x = if p x then [x] else []


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
-- version 2
{- Zależnosci rekurencyjne
M_n = n * srednia; S_n = n * wariacja
M_(n+1) = (n+1) * (srednia od n+1) = M_n +x_n+1
S_(n+1) = (n+1) * (wariacja od n+1) = (x_(n+1) - (srednia od n+1))^2 + suma(k=1)(n)((x_k - (srednia od n) + ((srednia od n) - (srednia od n+1)))^2) = 
    = 
-}
-- avgvar' :: Fractional a => [a] -> (a, a)
f' = \(m, s, n) x -> (m + x, (s + ((n * x - m)^2)/(n * (n + 1))), n + 1)
g' = \(m, s, n) -> (m/n, s/n)
avgvar' [] = (0.0, 0.0)
avgvar' (x:xs) = g' $ foldl f' (x, 0.0, 1) xs
-- version 3 z użyciem wartości oczekiwanej: var(X) = E(X^2) - E(X)^2
avgvar'' xs = (\(e_x, e_x2, n) -> ex/n, (((e_x)^2)-e_x2)/n) $ foldl (\(e_x, e_x2, n) x -> (e_x + x, e_x2 + x^2, n+1)) (0, 0, 0) xs

-- Zadanie 52

