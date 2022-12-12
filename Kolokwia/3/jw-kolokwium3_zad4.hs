-- Zadanie 4 - Korzystając z foldl lub foldr napisać średnią geometryczną z [a1..an], np:
-- geoMean [4,9] == 6.0
-- geoMean [1..9] == 4.1472

geoMean [] = 0.0
geoMean xs = (foldl (*) 1.0 xs) ** (1.0 / fromIntegral (length xs)) -- fromIntegral - funkcja konwersji

