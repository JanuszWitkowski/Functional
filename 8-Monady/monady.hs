-- Zadanie 69
-- data Pkt a = Pkt (a,a) deriving Show

-- instance Functor Pkt where
--     fmap f (Pkt (x, y)) = Pkt ((f x), (f y))

-- instance Applicative Pkt where
--     pure x = Pkt (x, x)
--     (Pkt (f, g)) <*> (Pkt (x, y)) = Pkt ((f x), (g y))

-- instance Monad Pkt where
--     return = pure
--     (Pkt (x, y)) >>= f = Pkt ((f x), (f y))

data Pkt a = Pkt a a deriving Show

instance Functor Pkt where
    fmap f (Pkt x y) = Pkt (f x) (f y)

instance Applicative Pkt where
    pure x = Pkt x x
    (Pkt f g) <*> (Pkt x y) = Pkt (f x) (g y)

instance Monad Pkt where
    return = pure
    (Pkt x y) >>= f = f x


data Tree a = Leaf a | Inner (Tree a) (Tree a)

{- mytree: do testów --}  
mytree1 = Inner (Inner (Leaf 1) (Leaf 2)) (Leaf 5)
mytree2 = Inner (Inner (Leaf 1) (Leaf 2)) (Leaf 5)
mytree3 = Inner (Leaf 1) (Inner (Leaf 2) (Leaf 5))

instance (Show a) => Show (Tree a) where
  show (Leaf x)     = show x
  show (Inner lt rt) =  "(" ++ show lt ++ "|" ++ show rt ++ ")"

-- 65.5
-- myfn :: (t -> Tree a) -> Tree 
myfn f (Leaf a) = f a
myfn f (Inner a b) = Inner (myfn f a) (myfn f b)


-- Zadanie 70
instance Functor Tree where
   fmap f (Leaf x) = Leaf (f x)
   fmap f (Inner x y) = Inner (fmap f x) (fmap f y)

instance Applicative Tree where
   pure = Leaf
   (Leaf f) <*> (Leaf x) = Leaf (f x)
   (Leaf f) <*> (Inner x y) = Inner (fmap f x) (fmap f y)
  --  (Inner f g) <*> (Leaf x) = Inner (Leaf (fmap f x)) (Leaf (fmap g x))
  --  (Inner f g) <*> (Inner x y) = Inner (fmap f x) (fmap g y)

instance Monad Tree where
    return = pure
    Leaf a >>= f = f a
    Inner lt rt >>= f = Inner (lt >>= f) (rt >>= f)

-- {-
tree1 = Inner (Leaf 3) (Inner (Leaf 4) (Leaf 5))
tree2 = Inner (Leaf "ABC") (Inner (Leaf "DEFG") (Leaf "HIJKL"))
{-
fmap (*2) tree1
fmap length tree2
(Leaf (*2)) <*> tree1
(Leaf init) <*> tree2
tree1 >>= (\x -> Leaf (x+200))
tree2 >>= (\x -> Leaf (tail x))
-}




-- MISC
data ErrEval = BadDom String | BadDiv String | BadIn String --deriving Show

instance Show ErrEval where
    show (BadDom str) = show ("[BAD DOMAIN ERROR] " ++ str)
    show (BadDiv str) = show ("[BAD DIVISION ERROR] " ++ str)
    show (BadIn str) = show ("[BAD INPUT ERROR] " ++ str)

safeSqrt :: Float -> Either ErrEval Float
safeSqrt x = if x < 0 then Left (BadDom "Sqrt z liczby ujemnej.") else Right (sqrt x)

safeLog :: Float -> Either ErrEval Float
safeLog x = if x < 0 then Left (BadDom "Log z liczby niedodatniej.") else if x == 0 then Left (BadDiv "Dzielenie przez zero w logarytmie.") else Right (log x)

safeInverse :: Float -> Either ErrEval Float
safeInverse x = if x == 0 then Left (BadDiv "Dzielenie przez zero w odwrotności.") else Right (1.0 / x)

safeHead :: [a] -> Either ErrEval a
safeHead xs = if (null xs) then Left (BadIn "Pusta lista.") else Right (head xs)


-- Inne monady
data Id a = Id a deriving Show

instance Functor Id where
    fmap f (Id x) = Id (f x)

instance Applicative Id where
    pure x = Id x
    (Id f) <*> (Id x) = Id (f x)

instance Monad Id where
    return = pure
    (Id x) >>= f = f x


-- Inne monady (z copilota xd)
-- 1. Maybe
-- 2. Either
-- 3. IO
-- 4. State
-- 5. Reader
-- 6. Writer
-- 7. Cont
-- 8. List
-- 9. Tree
-- 10. ZipList
-- 11. Array
-- 12. Complex
-- 13. Ratio
-- 14. Set
-- 15. Map
-- 16. Seq
-- 17. Identity
-- 18. Proxy
-- 19. Const
-- 20. Dual
-- 21. Endo
-- 22. First
-- 23. Last
-- 24. Product
-- 25. Sum
-- 26. WrappedMonad
-- 27. ZipStream
-- 28. NonEmpty
-- 29. IdentityT
-- 30. MaybeT


