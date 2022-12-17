module Main where
 
import System.Random
 
main :: IO ()
main = do putStrLn "Sprobuj odgadnac liczbe ze zbioru {0, 1, ... , 123}"
          sekret <- randomRIO (0, 123)
          graj sekret
 
 
graj :: Int -> IO ()
graj sekret = do 
  liczbaProb <- grajGre sekret 0
  putStrLn $ "Wygrales ! Zajelo Ci to " ++ show liczbaProb ++ " prob !"
 
 
grajGre :: Int -> Int -> IO Int
grajGre sekret liczbaProb = do 
  putStr "? "
  input <- getLine
  let guess = read input :: Int
  if guess == sekret then do 
    return (liczbaProb + 1)
  else if guess < sekret then do 
    putStrLn "Za mala !"
    grajGre sekret (liczbaProb + 1)
  else do 
    putStrLn "Za duza !"
    grajGre sekret (liczbaProb + 1)   
    