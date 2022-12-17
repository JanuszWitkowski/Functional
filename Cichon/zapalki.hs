module Zapalki (graj, play) where

import System.Random


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
 
data KtoTerazGra = User | Computer deriving Eq
 
graj :: IO ()
graj = do 
  start <- randomRIO (10,15) -- poczatkowa liczba zapalek
  putStr (gameInfo start)
  kto <- play User start
  if kto == User then putStrLn "WYGRALES !!!"
                 else putStrLn "Przegrales !!!"  
  
play:: KtoTerazGra -> Int -> IO KtoTerazGra   
play User stan = do 
  if stan == 0 then return User 
  else do
    putStrLn $ gameStateInfo stan
    putStr  "TWOJ ruch: wybierz liczbe od 1 to 3: "  
    input<- getLine
    let wybor = read input :: Int
    if (wybor<=0) || (wybor>=4) || (wybor>stan) then do 
      putStrLn "BLAD: Podales zla liczbe !\nSprobuj ponownie"
      play User stan
    else if stan == wybor then -- user wzial ostatnie zapalki 
      return User              -- wygral User
    else do 
      play  Computer (stan - wybor)
play Computer stan = do
  putStrLn $ gameStateInfo stan
  let wybor = mojWybor stan 
  putStrLn $ mojWyborInfo wybor
  if stan == wybor then return Computer -- wygral Computer
                   else play User (stan-wybor)   

      
         
    
