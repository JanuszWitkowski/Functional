{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
module Zapalki (graj) where

import System.Random
import Control.Monad.Writer

gameInfo :: Int -> String
gameInfo start = unlines [
  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>",
  "Mamy " ++ show start ++ " zapalek",
  "Mozesz w kazdym ruchu zabrac 1, 2 lub 3 zapalki.",
  "Wygra ten z nas który zabierze ostatnia zapalke.",
  "Zaczynamy."
  ]

gameStateInfo :: Int -> String
gameStateInfo stan = "Liczba zapalek: " ++ show stan ++ "."

mojWyborInfo :: Int -> String
mojWyborInfo wybor =  "MOJ wybor: Biorę " ++ show wybor ++ "."

mojWybor stan = let good = mod stan 4 in
                if good>0 then good
                          else 1
-- funkcja wyspecjalizowana w czytaniu liczb ze zbioru {1,2,3} 
read123 :: IO Int
read123 = do
  input<- getLine
  case reads input :: [(Int,String)] of
    [(n, _)]  -> if (n>=1) && (n<=3) then return n
                 else do putStrLn "Podaj liczbę ze zbioru {1,2,3}"
                         read123
    _         -> do putStrLn "Podaj poprawną liczbę ze zbioru {1,2,3}"
                    read123

type GameWriter a = WriterT [Int] IO a
 
data KtoTerazGra = User | Computer deriving Eq
 
graj :: GameWriter ()
graj = do 
  start <- liftIO $ randomRIO (10,15) -- poczatkowa liczba zapalek
  tell [start]
  liftIO $ putStr (gameInfo start)
  kto <- play User start
  if kto == User then liftIO $ putStrLn  "WYGRALES !!!"
                 else liftIO $ putStrLn  "Przegrales !!!"  
  

play :: KtoTerazGra ->Int -> GameWriter KtoTerazGra
play User stan = do  -- teraz gra uzytownik
  if stan == 0 then return User 
  else do
    liftIO (putStrLn $ gameStateInfo stan)
    liftIO (putStr "TWOJ ruch: wybierz liczbe od 1 to 3: ")  
    wybor <- liftIO read123
    if  wybor>stan then do 
      liftIO $ putStrLn "BLAD: Podales zla liczbe !\nSprobuj ponownie."
      play User stan
    else if stan == wybor then do -- user wzial ostatnie zapalki 
      tell [0];
      return User                 -- wygral User
    else do 
      tell [stan-wybor]
      play  Computer (stan - wybor)
play Computer stan = do  -- teraz gra komputer
  liftIO $ putStrLn  (gameStateInfo stan)
  let wybor = mojWybor stan 
  liftIO $ putStrLn (mojWyborInfo wybor)
  if stan == wybor then do {tell [0];            return Computer} -- wygral Computer
                   else do {tell [stan-wybor]; play User (stan-wybor)}   

  
         
    
