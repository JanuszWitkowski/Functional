-- Zadanie 8
fnc8 x y = (\z -> (\w -> z + 2 * w))(x * y)
fnc8' x y = \w -> x * y + 2 * w

