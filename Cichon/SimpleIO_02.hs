{-
readFile   :: FilePath -> IO String
writeFile  :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()

type FilePath = String
-}

import Data.Char
import Data.List
import GHC.Exts

toLowerCase = map toLower

filterChars = filter (\c -> not (elem c ".;!/"))
filterWords = filter (\w -> length w>3) 
tr1 = group. sort . filterWords . words . filterChars . toLowerCase 
tr2 = sortBy (\x y -> compare (length y) (length x)) 
finall txt  = unwords $ (map head) (take 100 ((tr2.tr1) txt) ) 


main = do
  txt <- readFile "Balladyna.txt"
  writeFile "Ball_Converted.txt" $ finall txt 
