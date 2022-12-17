{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{- # LANGUAGE PolyKinds #-}
{- # LANGUAGE DataKinds #-}
{-
  Monada State
  Podstawowy pomysł: (
-}

import Data.Semigroup

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State st) = State (\s -> let (x,t) = st s in (f x,t) )
  
instance Monad (State s) where
  return x = State (\s -> (x,s))
  (State st) >>= f = State $ \s -> let (x,t) = st s
                                       (State st') = f x
                                   in st' t  
instance Applicative (State s) where
  pure = return
  State fx <*> State sx = do {f <- State fx; x <- State sx; return (f x)}  
  {-- TODO:: samemu obliczyć --}
  
class Monad m => MonadState s m| m -> s where
   get :: m s        -- | Return the state from the internals of the monad.
   put :: s -> m ()  -- | Replace the state inside the monad.
   state :: (s->(a,s)) -> m a
    
   
instance MonadState s (State s) where
  get     = State (\s -> (s,s))
  put s   = State (\_ -> ((),s))
  state f = State f
  
-- Automat akceptujący wyrażenie
-- {a+b}*ab*

data StanyAutomatu = S1 | S2 | S3 deriving (Eq,Show)


delta 'a' S1 = ("1>2;", S2)
delta 'a' S2 = ("2>1;", S2)
delta 'b' S1 = ("1>1;", S1)
delta 'b' S2 = ("2>2;", S2)
delta  _  _ =  ("?;"  , S3)
  
runAUT :: String -> State StanyAutomatu String
runAUT [] = State(\s -> ([],s))
runAUT (c:cs) = do
  a <- State (delta c)
  b <- runAUT cs
  return (a ++ b)

runAUT' :: String -> State StanyAutomatu String
runAUT' [] = State(\s -> ([],s))
runAUT' (c:cs) = do
  if elem c "ab" then do a <- State (delta c)
                         b <- runAUT' cs
                         return (a ++ b)
  else do {b<-runAUT' cs; return ("?;"++b)}
  
runAUT'' :: String -> State StanyAutomatu String
runAUT'' [] = State(\s -> ([],s))
runAUT'' (c:cs) = do
  a <- if elem c "ab" then State (delta c) 
                      else return "?;"
  b <- runAUT' cs
  return (a ++ b)
 
runTest cs = sequence $ map(\f->State f) $ map (\x->delta x) cs
 
{-- AUTOMAT ROBIACY KAWĘ --}

data StanyM = Locked | Unlocked deriving (Eq, Show)
data AkcjeM = Dziekuje| Otwarty | Dzwonek deriving (Eq, Show)

moneta, przycisk :: StanyM -> (AkcjeM, StanyM)

moneta   _        = (Dziekuje,Unlocked)
przycisk Locked   = (Dzwonek, Locked)
przycisk Unlocked = (Otwarty, Locked) 

monetaS   = State moneta
przyciskS = State przycisk

-- przyklad uzycia: runState monetaS Locked

poniedzialek = do
  a1 <- monetaS
  a2 <- przyciskS
  a3 <- przyciskS
  a4 <- monetaS
  a5 <- przyciskS
  return [a1,a2,a3,a4,a5]

 -- sequence [monetaS, przyciskS, ....]
 
{-- Ewaluator stosowy --}

push :: Integer -> State [Integer] Integer
push n = State $ \s -> (n, n:s)

pop :: State [Integer] Integer
pop = State $ \(x:xs) -> (x,xs)

top :: State [Integer] Integer
top = State $ \(x:xs) -> (x,x:xs)

--kilka funkcji arytmetycznych
plus, mult :: State [Integer] Integer
plus = do 
  x <- pop
  y <- pop
  push (x+y)
mult = do
  x<- pop
  y<- pop
  push (x*y)

prog = do 
  push 3
  push 5
  push 2
  x<- pop
  y<- pop
  push (x-y)
  mult
  push 1
  plus
  pop

-- teraz to samo, ale z wykorzystaniem "utilities"
-- push :: Integer -> State [Integer] Integer
-- push n = State $ \s -> (n, n:s)
push' :: Integer -> State [Integer] ()
push' n = do {stos<- get; put (n:stos)}

-- pop = State $ \(x:xs) -> (x,xs)
pop' :: State [Integer] Integer
pop' = do {stos <- get; put (tail stos); return (head stos) }

-- top = State $ \(x:xs) -> (x,x:xs)
top' :: State [Integer] Integer
top' = do {stos<- get; return (head stos)}

--kilka funkcji arytmetycznych
plus', mult' :: State [Integer] ()
plus' = do 
  x <- pop'
  y <- pop'
  push' (x+y)
mult' = do
  x<- pop'
  y<- pop'
  push' (x*y)

prog' = do 
  push' 3
  push' 5
  push' 2
  x<- pop'
  y<- pop'
  push' (x-y)
  mult'
  push' 1
  plus'
  pop'

  