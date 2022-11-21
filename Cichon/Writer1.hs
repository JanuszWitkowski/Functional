{- Druga wersja monady Writer
   Teraz będziemy mogli "pisać" w dowolnym monoidzie
-}

import Data.Semigroup

newtype Writer w a = Writer (a,w) deriving Show

instance Functor (Writer w) where
  fmap f (Writer (x,s)) = Writer (f x, s)
  
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x,w)) >>= f = let Writer (y,t) = f x in Writer (y, w <> t)
  
instance (Monoid w) => Applicative (Writer w) where
  pure = return
  Writer (f, m) <*> Writer (x, n) = Writer (f x, m <> n)
  {-- ulepszyliśmy implementację z pliku writer0.hs
      zrobiliśmy to samodzielnie (czyli "na kartce papieru") przeliczając kod z poprzzedniego pliku
  --}  

---------------------------------------------------
-- Przykład użycia 
-- ewaluator "przy okazji obliczeń" ma przekształcić wyrażanie 
-- w odwrotną postać polską

data Term = C Float     -- Constant
          | A Term Term -- Add
          | S Term Term -- Substract
          | M Term Term -- Multiply
          | D Term Term -- Divide

-- kilka termów do testów
  
termA = D (A (C 1)(C 2) ) (M (C 2) (C 3))
termB = D (A (C 1)(C 2) ) (M (C 2) (C 0))
termC = M termA (S (C 5) (C 6))

-- to jest klasyczny ewaluator
-- który zaraz przekształcimy w ewaluator monadyczny 

eval0:: Term -> Float  
eval0 (C x)     = x
eval0 (A t1 t2) = let x = eval0 t1; y = eval0 t2 in (x+y)
eval0 (S t1 t2) = let x = eval0 t1; y = eval0 t2 in (x-y)
eval0 (M t1 t2) = let x = eval0 t1; y = eval0 t2 in (x*y)
eval0 (D t1 t2) = let x = eval0 t1; y = eval0 t2 in (x/y)

-- ewaluator monadyczny 

eval:: Term -> Writer String Float  
eval (C x)     = Writer (x,"("++show x++")")
eval (A t1 t2) = do { x <- eval t1; y <- eval t2 ; Writer (x+y, "+")}
eval (S t1 t2) = do { x <- eval t1; y <- eval t2 ; Writer (x-y, "-")}
eval (M t1 t2) = do { x <- eval t1; y <- eval t2 ; Writer (x*y, "*")}
eval (D t1 t2) = do { x <- eval t1; y <- eval t2 ; Writer (x/y, "/")}

-- przykład użycia: eval termA 
