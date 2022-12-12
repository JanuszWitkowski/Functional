-- Zadanie 5 - Usuwanie z listy liczb naturalnych 2-gich potęg liczb całkowitych, np:
-- removeSquares [1,2,4,5,9] == [2,5]

-- Funckja filtrująca, która dla każdego elementu x sprawdza liczby [1..(x/2 + 1)]
removeSquares = filter (\x -> if x <= 0 then True else checkIfNotSquare x [1..((div x 2) + 1)]) where
    checkIfNotSquare _ [] = True
    checkIfNotSquare yy (y:ys) = if y*y == yy then False else checkIfNotSquare yy ys


