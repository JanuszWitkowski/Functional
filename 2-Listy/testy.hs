{-  PROGRAMOWANIE FUNKCYJNE
    2. LISTY
    by Janusz Witkowski     -}


-- Zadanie 27
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs


-- Zadanie 28   ???
-- mysum
-- sum' xs  =  let { ys = 0 : map (\(a,b) -> a + b) (zip xs ys) } in last ys
mysum :: Num a => [a] -> a
-- why does it work???
mysum xs = let { ys = 0 : mymap (\(a,b) -> a + b) (zip xs ys) } in last ys

-- myproduct
myproduct :: Num a => [a] -> a
myproduct xs = let { ys = 1 : mymap (\(a,b) -> a * b) (zip xs ys) } in last ys

-- myfact
myfact :: (Num a, Enum a) => a -> a
myfact n = let { ys = 1 : mymap (\(a,b) -> a * b) (zip [1..n] ys) } in last ys
fact n = if n == 0 then 1 else (fact (n-1)) * n


-- Zadanie 29


-- Zadanie 32
-- partitions (x0:xs1:xm:xs2) = [(ys,zs) | ys <- (x0:xs1), zs <- (xm:xs2)]


-- Zadanie 33
nondec [] = True
nondec [x] = True
nondec (x1:x2:xs) = if x1 > x2 then False else nondec (x2:xs)


-- Zadanie 36
-- A trailing zero is created IFF a single 2 and a single 5 are multiplied together.
-- Idea: Count the number of times a 5 is multiplied (number of 2s is never smaller than number of 5s).
-- version 1
howmanymultiples n m = length [a | a <- [1..n], mod a m == 0]
multiplespowers n m b = if m > n then 0 else (howmanymultiples n m) + (multiplespowers n (m*b) b)
trailingzeros n = multiplespowers n 5 5

