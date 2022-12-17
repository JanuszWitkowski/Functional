
module Zapalki (graj) where

import System.Random


gameInfo :: Int -> String
gameInfo start = unlines [
  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>",
  "Mamy " ++ show start ++ " zapalek",
  "Mozesz w kazdym ruchu zabrac 1, 2 lub 3 zapalki.",
  "Wygra ten z nas który zabierze ostatnia zapalke.",
  "Zaczynamy."
  ]

gameStateStr :: Int -> String
gameStateStr stan = "Liczba zapalek: " ++ show stan ++ "."

mojWyborStr :: Int -> String
mojWyborStr wybor =  "MOJ wybor: Biorę " ++ show wybor ++ "."

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

data KtoTerazGra = User | Computer deriving Eq
 
graj :: IO ()
graj = do 
  start <- randomRIO (10,15) -- poczatkowa liczba zapalek
  putStr (gameInfo start)
  zwyciezca <- play User start
  if zwyciezca == User then putStrLn "WYGRALES !!!"
                       else putStrLn "Przegrales !!!"  

  
play:: KtoTerazGra -> Int -> IO KtoTerazGra   
play User stan = do 
  if stan == 0 then return User 
  else do
    putStrLn $ gameStateStr stan
    putStr  "TWOJ ruch: wybierz liczbe od 1 to 3: "  
    wybor <- read123
    if wybor>stan then do 
      putStrLn "BLAD: Podales zla liczbe !\nSprobuj ponownie"
      play User stan
    else if stan == wybor then -- user wzial ostatnie zapalki 
      return User              -- wygral User
    else do 
      play  Computer (stan - wybor)
play Computer stan = do
  putStrLn $ gameStateStr stan
  let wybor = mojWybor stan 
  putStrLn $ mojWyborStr wybor
  if stan == wybor then return Computer -- wygral Computer
                   else play User (stan-wybor)   

      
         
    
