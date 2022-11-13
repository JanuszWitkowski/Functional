import Data.List

-- Zadanie 61
findQueens n = [perm | perm <- permutations [1..n], nodiagonalcaptures perm]
    where nodiagonalcaptures [] = True
          nodiagonalcaptures (x:xs) = (diagonalcheck x xs 1) && (nodiagonalcaptures xs)
          diagonalcheck _ [] _ = True
         --  diagonalcheck e (y:ys) d = if y == e + d || y == e - d then False else diagonalcheck e ys (d+1)
          diagonalcheck e (y:ys) d =  (y /= e + d) && (y /= e - d) && (diagonalcheck e ys (d+1))

-- _hetmani = [perm | perm <- perms, nocaptures perm]
--     where perms = permutations [1..8]
--           nocaptures pm = (nodiagonalcaptures pm) && (nodiagonalcaptures (reverse pm))
--           nodiagonalcaptures [] = True
--           nodiagonalcaptures (x:xs) = (diagonalcheck x xs 1) && (nodiagonalcaptures xs)
--           diagonalcheck _ [] _ = True
--           diagonalcheck e (y:ys) d = if y == e + d || y == e - d then False else diagonalcheck e ys (d+1)

hetmani = findQueens 8

filterEquivalent lls = let makecycle [] cycletail = cycletail
                           makecycle (x:xs) cycletail = if x == 1 then (x:xs) ++ cycletail else makecycle xs (cycletail ++ [x])
                           invariant xs = snd $ foldl (\(prev, seq) next -> (next, seq ++ [prev - next])) (last xs, []) xs
                        --    invariant xs = makecycle xs []
                           isNotEquivalentToOthers _ [] = True
                           isNotEquivalentToOthers permInvariant (o:others) = if permInvariant == (invariant o) then False else isNotEquivalentToOthers permInvariant others
                       in foldl (\xxs perm -> if isNotEquivalentToOthers (reverse $ invariant perm) xxs then (perm:xxs) else xxs) [] lls
                    --    in foldl (\xxs perm -> if isNotEquivalentToOthers (invariant perm) xxs then ((makecycle perm []):xxs) else xxs) [] lls


nierownowazni_hetmani = filterEquivalent hetmani

-- version 2
-- _hetman n = [p | p <- permutations [1..n], notDiag p n]
--     where notDiag [] _ = True
--           notDiag (x:xs) n = notSingleDiag x xs

-- reverseY xs n = map (\z -> n+1 - z) xs
-- isSimilar x y n = (x == reverse y) || (x == reverseY y n) || (x == reverseY (reverse y) n)
-- isNotInside _ [] = True
-- isNotInside k (x:xs) = if isSimilar k x then False else isNotInside k xs
-- nonSimilarH [] = []
-- nonSimilarH (x:xs) = if isNotInside x xs then x:(nonSimilarH xs) else nonSimilarH xs

-- version 3
mtHetman n i a b c | i < n = concat [mtHetman n (i+1) (a ++ [j]) (b ++ [j + i]) (c ++ [i - 1]) | j <- [0..(n-1)], not (elem j a), not (elem (i+j) b), not (elem (i-j) c)]
                   | otherwise = [a]
-- mtHetman 8 0 [] [] []


-- Zadanie 63


