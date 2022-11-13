{-  PROGRAMOWANIE FUNKCYJNE
    2. LISTY
    by Janusz Witkowski     -}

import Data.List    -- for "delete" function, used in Zadanie 35

-- Zadanie 27
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs


-- Zadanie 28
-- mysum
-- sum' xs  =  let { ys = 0 : map (\(a,b) -> a + b) (zip xs ys) } in last ys
mysum :: Num a => [a] -> a
-- mysum xs = let { ys = 0 : mymap (\(a,b) -> a + b) (zip xs ys) } in last ys
mysum xs = last ys
    where ys = 0 : mymap (\(a,b) -> a + b) (zip xs ys) 

mysumi' [a] = [a]
mysumi' (x:xs) = mymap (+x) (mysumi' xs)
mysum' xs = head (mysumi' xs)

-- myproduct
myproduct :: Num a => [a] -> a
myproduct xs = let { ys = 1 : mymap (\(a,b) -> a * b) (zip xs ys) } in last ys

-- myfact
myfact :: (Num a, Enum a) => a -> a
myfact n = let { ys = 1 : mymap (\(a,b) -> a * b) (zip [1..n] ys) } in last ys
fact n = if n == 0 then 1 else (fact (n-1)) * n


-- Zadanie 29
-- version 1
mynubhelp [] ys = ys
mynubhelp (x:xs) ys = if x `elem` ys then mynubhelp xs ys else mynubhelp xs (ys ++ [x])
mynub xs = mynubhelp xs []
-- version 2
mynub' [] = []
mynub' (x:xs) = if x `elem` xs then mynub' xs else x : mynub' xs
-- version 3
mynub'' xs = foldl (\li -> \x -> if x `elem` li then li else li ++ [x]) [] xs


-- Zadanie 30
-- version 1
myinits [] = [[]]
myinits xs = (myinits (init xs)) ++ [xs]
-- version 2
myinits' xs = let { ys = ([]) : mymap (\(as, b) -> as ++ [b]) (zip ys xs) } in ys
-- version 3
myinits'' xs = [take t xs | t <- [0..(length xs)]]


-- Zadanie 31
-- version 1
mytails [] = [[]]
mytails xs = (mytails (tail xs)) ++ [xs]
-- version 2
mytails' [] = [[]]
mytails' xs = xs : (mytails (tail xs))


-- Zadanie 32
partitions xs = zip (myinits xs) (reverse (mytails xs))


-- Zadanie 33
nondec [] = True
nondec [x] = True
nondec (x1:x2:xs) = if x1 > x2 then False else nondec (x2:xs)


-- Zadanie 34
-- version 1
myzip' (y:ys) [] = []
myzip' [] (z:zs) = []
myzip' [y] [z] = [(y, z)]
myzip' (y:ys) [z] = [(y, z)]
myzip' [y] (z:zs) = [(y, z)]
myzip' (y:ys) (z:zs) = [(y, z)] ++ (myzip' ys zs)
-- version 2
myzip _ [] = []
myzip [] _ = []
myzip (y:ys) (z:zs) = (y, z) : zip ys zs


-- Zadanie 35
mypermutations [] = [[]]
mypermutations as = [a:x | a <- as, x <- mypermutations $ delete a as]
-- TODO: write myperms without "delete" function.


-- Zadanie 36
-- A trailing zero is created IFF a single 2 and a single 5 are multiplied together.
-- Idea: Count the number of times a 5 is multiplied (number of 2s is never smaller than number of 5s).
-- version 1
howmanymultiples n m = length [a | a <- [1..n], mod a m == 0]
multiplespowers n m b = if m > n then 0 else (howmanymultiples n m) + (multiplespowers n (m*b) b)
trailingzeros n = multiplespowers n 5 5


-- Zadanie 37
qs [] = []
qs (x:xs) = (qs [k | k <- xs, k <= x]) ++ [x] ++ (qs [k | k <- xs, k > x])
qs' [] = []
qs' [x] = [x]
qs' [x1, x2] = if x1 <= x2 then [x1, x2] else [x2, x1]
qs' (x:xs) = (qs' [k | k <- xs, k <= x]) ++ [x] ++ (qs' [k | k <- xs, k > x])


-- Zadanie 38
mmap f = map (map f)
mmmap f = map (map (map f))
mmap' = map . map
mmmap' = map . map . map
{-
38.1
ghci> :t mmap
mmap :: (a -> b) -> [[a]] -> [[b]]
ghci> :t mmmap
mmmap :: (a -> b) -> [[[a]]] -> [[[b]]]
ghci> :t mmap'
mmap' :: (a -> b) -> [[a]] -> [[b]]
ghci> :t mmmap'
mmmap' :: (a -> b) -> [[[a]]] -> [[[b]]]

38.2

38.3
Kropka symbolizuje w Haskellu złożenie funkcji.
ghci> mmap (*2) y
[[2,4],[6,8],[10,12]]
ghci> mmap' (*2) y
[[2,4],[6,8],[10,12]]

ghci> sq x = x * x
ghci> db x = x + x
ghci> (sq . db) 7
196
ghci> (db . sq) 7
98
ghci> sq (db 7)
196
ghci> db (sq 7)
98
-}

