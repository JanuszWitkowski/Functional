zadanie3 n = length [k | k <- [1..n], (isPrime k (div k 2))]    -- Zwroc dlugosc listy liczb, ktore nie dziela sie przez zadna liczbe od 2 do polowy samej siebie.
    where isPrime p q = if p == 1 then False    -- 1 nie jest l. pierwsza.
          else if q == 1 then True              -- Jezeli doszlismy do dzielnika rownego 1, oznacza to ze liczba p nie podzielila sie przez nic innego.
          else if p `mod` q == 0 then False     -- Jesli p sie przez cos podzieli, to p nie moze byc pierwsza.
          else isPrime p (q-1)                  -- Rekurencyjnie sprawdzamy kolejny dzielnik.

-- zadanie3 30 == 10    {2, 3, 5, 7, 11, 13, 17, 19, 23, 29}
-- zadanie3 31 == 11    {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31}
