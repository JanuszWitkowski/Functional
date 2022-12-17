import Data.Char
import System.IO  -- main2

{- OUTPUT FUNCTIONS 

  putChar :: Char -> IO ()
  putStr :: String -> IO ()
  putStrLn :: String -> IO ()
  
  print :: Show a => a -> IO ()
  print x  =  putStrLn (show x)

  example:  main = print ([(n, 2^n) | n <- [0..10]])
-}

{- INPUT FUNCTIONS 
  getChar :: IO Char
  getLine :: IO String
  readLn  :: Read a => IO a
  
  getContents :: IO String
  interact :: (String -> String) -> IO ()
-}

----------------------------------

main0 = do
  putStrLn "Hello word"
 
----------------------------------
toUpperCase = map toUpper 

main1 = do 
  putStr "Podaj imie "
  txt <- getLine
  putStrLn ("Witaj " ++ toUpperCase txt)
  
-----------------------------------

fib = 0:1: zipWith (+) fib (tail fib)

fibN n = if n<=0 then 0 
         else fib !!n
         
main = do
  putStrLn "Obliczę ci liczbę Fibonaciego"
  putStr   "Podaj n: "
  n <- readLn :: IO Int
  putStrLn ("F(" ++ show n ++ ") = " ++ show (fibN n))
  
------------------------------------

{-
prompt :: Read b => String -> IO b
prompt txt = do
    putStr txt
    hFlush stdout
    readLn

main2 = do
  putStrLn "Obliczę ci liczbę Fibonaciego"
  n <- prompt "Podaj n: " :: IO Int
  putStrLn ("F(" ++ show n ++ ") = " ++ show (fibN n))
  print ([(n, 2^n) | n <- [0..10]])

-}

copyUntilEmpty = do
  line<- getLine
  if line == "" then return ()
  else (do putStrLn (reverse line)
           copyUntilEmpty
        )
        
main2 = do
  putStrLn "Obliczę ci liczbę Fibonaciego"
  putStr   "Podaj n: "
  n <- readLn :: IO Int
  if n<0 then do
    putStrLn "Do widzenia"
    return ()
  else do
    putStrLn ("F(" ++ show n ++ ") = " ++ show (fibN n))
    main2
    
main3 ile = do
  if ile==0 then putStrLn "Obliczę ci liczbę Fibonaciego"
            else return ()  
  if ile==0 then putStr "Podaj n: "
            else putStr "Podaj kolejne n: "
  n <- readLn :: IO Int
  if n<0 then do
    putStrLn "Do widzenia"
    return ()
  else do
    putStrLn ("F(" ++ show n ++ ") = " ++ show (fibN n))
    main3 (ile+1)
 