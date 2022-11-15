combinationNumber n xs = d a b
  where (a, b) = (n, (length xs - 1))
        xs' = listArray (0, b) xs
        d i 0
          | i `mod` (xs' ! 0) == 0 = 1
          | otherwise = 0
        d 0 j = 1
        d i j
          | i - (xs' ! j) >= 0 = d (i - (xs' ! j)) j + d i (j - 1)
          | otherwise = d i (j - 1)
