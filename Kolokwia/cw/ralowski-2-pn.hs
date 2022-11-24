-- 1. Napisać funkcję mytails, jak z list zadań
-- Zadanie 31
-- version 1
mytails [] = [[]]
mytails xs = (mytails (tail xs)) ++ [xs]
-- version 2
mytails' [] = [[]]
mytails' xs = xs : (mytails (tail xs))

-- 2. Napisać funkcję, która wyznaczy wszystkie podciągi (niekoniecznie spójne) wejściowej listy np: f [1,2] = [[], [1], [2], [1,2]]
myseqs [] = [[]]
myseqs (x:xs) = let seqs = myseqs xs in (map (x:) seqs) ++ seqs
{- Wywołanie:
myseqs [1,2]
[[1,2],[1],[2],[]]
myseqs [1..3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
-}

-- 3. Przy pomocy foldr lub foldl napisać funkcję która wyliczy sumę 1 / (i+1)^2 po i od 0 do n
mysum n = foldl (\e i -> e + (1 / ((i + 1)^2))) 0 [0..n]

