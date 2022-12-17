put3times str = do
  putStrLn str
  putStrLn str
  putStrLn str
  
putNtimes :: Int -> String -> IO()
putNtimes n str = 
  if n<=1 then putStrLn str
  else do putStrLn str
          putNtimes (n-1) str   
          
reverseLines = do
  line1 <- getLine
  line2 <- getLine
  putStrLn (reverse line1)
  putStrLn (reverse line2)
  
reverseLines' = do
  putStr "Linia 1: "
  line1 <- getLine
  putStr "Linia 2: "
  line2 <- getLine
  let r1 = reverse line1
  let r2 = reverse line2
  putStrLn (r1)
  putStrLn (r2)
  
getInt n = do line<- getLine
              return (read line::Int)

copyUntilEmpty = do
  line<- getLine
  if line == "" then return ()
  else (do putStrLn (reverse line)
           copyUntilEmpty
        )
        