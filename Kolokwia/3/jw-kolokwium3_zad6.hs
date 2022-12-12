-- Zadanie 6 - Dla n,k wyznaczyć listę wszystkich ciągów rosnących listy [1..n] długości k, np:
-- comb 3 2 == [[1,2],[2,3],[1,3]]

comb n k = findCombs [1..n] k where
    findCombs [] _ = []
    findCombs (x:xs) c = if c <= 0 then [] else if c > (length (x:xs)) then [] else (findCombs xs (c)) ++ (x:(findCombs xs (c-1)))

