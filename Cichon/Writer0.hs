{- Pierwsza przymiarka do oprogramowania Monady Writer 
   "Pisać" będziemy w monoidzie (String, ++, "")
-}

data Writer a = Writer (a,String) deriving Show
 
instance Functor Writer where
  fmap f (Writer (x,s)) = Writer (f x, s)
  
instance Monad Writer where
  return x = Writer (x, "")
  (Writer (x,s)) >>= f = let Writer (y,t) = f x in Writer (y, s++t)
                       
instance Applicative Writer where
  pure = return
  fw <*> xw = do f<- fw; x<- xw; return( f x)
  {-- to jest bardzo niechluje rozwiązanie 
      tę prowizorkę naprawimy w następnym kroku
  --}
  
---------------------------------------------------

half::Int -> Writer Int
half x = Writer (x `div` 2, "Polowie " ++ show x ++ ";")

-- użycie:  half 100 >>= half >>= half >>= half
