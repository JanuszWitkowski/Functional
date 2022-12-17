{- 
  Konstrukcja funkcji wyznaczającej najczęściej występujące 
  słowa w tekście
--}

import Data.List(group, sort, sortBy)
import Data.Char(toLower)

dopuszczalne = [' ']++['a'..'z']

stopWords    = ["a","an","and","are", "at", "as","all",
                "be", "by", "do", "it", "in", "no", "ll", 
                "so", "s", "o", "or", "t", "th", "this","that","the","thy",
                "i", "it", "he", "his", "me",
                "is","to","thou","tis",
                "you","your","our",  
                "we","will","was"]  -- dosyć przypadkowe

txt2Lower = map toLower

oczyscZnaki = map (\c -> if elem c dopuszczalne then c else ' ') 

oczyscSlowa = filter (\slowo -> not(elem slowo stopWords))
 
zlicz = map (\l -> (head l, length l))

posortuj = sortBy (\x y -> compare (snd y) (snd x))  

przeksztalc =  posortuj 
               . zlicz 
               . group -- grupuje sąsiednie elementy :: [a] -> [[a]]
               . sort -- aby poprawnie móc zastosować group
               . oczyscSlowa
               . words
               . oczyscZnaki 
               . txt2Lower  

mostFrequent n xs = map fst (take n ( przeksztalc xs))

{- 
  Po raz pierwszy na wykładzie komunikujemy się ze światem zewnętrznym.
  Omówimy to w przyszłości dokładniej
-}
main = do
  txt <- readFile "hamlet.txt"
  putStrLn (show (mostFrequent 50 txt))