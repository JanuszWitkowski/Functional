{-  PROGRAMOWANIE FUNKCYJNE
    2. LISTY
    by Janusz Witkowski     -}


-- Zadanie 27
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs


-- Zadanie 28   ???
-- mysum :: [a] -> a
-- mysum li = mymap () li

-- myproduct

-- myfact :: (num a) => a -> a
-- myfact n = 


-- Zadanie 29

