-- Korzystając z funkcji foldl lub foldr napisz funkcję liczb naturalnych bez zera zwraca sum(i=1..n)(1/(i^3))
-- mysum :: Int -> Double
mysum n = if n <= 0 then (-1) else foldl (\acc i -> acc + (1 / (i^3))) 0.0 [1..n]   -- Prosty foldl którego funkcja bierze akumulator i dodaje odpowiedni składnikk sumy

