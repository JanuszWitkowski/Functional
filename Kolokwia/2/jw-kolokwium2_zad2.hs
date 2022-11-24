-- Napisz funkcję która dla liczby naturalnej n wyświetla wszystkie permutacje [1..n] które nie mają punktów stałych,
-- np. na 3. elemencie listy nie może być liczby 3

aix n = if n <= 0 then [[]] else filterAll n (myperms [1..n]) where
    myperms [] = [[]]
    myperms xs = [x:p | x <- xs, p <- myperms (filter (/= x) xs)]   -- Znajduje wszystkie możliwe permutacje;
    filterAll i xs = if i <= 0 then xs else filterAll (i-1) (filter (\arg -> czyPunktStaly i arg) xs)    -- Wykonuje filtrację dla i=1..n na permutacjach
    czyPunktStaly m xs = if (m + (length xs) - 1) /= n then czyPunktStaly m (tail xs) else if m == (head xs) then False else True   -- Obcina permutację i patrzy czy na odpowiednim miejscu jest niewłaściwa wartość

