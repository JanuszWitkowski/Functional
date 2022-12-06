-- Zadanie 65
data Tree a = Leaf a | Inner (Tree a) (Tree a)

{- mytree: do testów --}  
mytree1 = Inner (Inner (Leaf 1) (Leaf 2)) (Leaf 5)
mytree2 = Inner (Inner (Leaf 1) (Leaf 2)) (Leaf 5)
mytree3 = Inner (Leaf 1) (Inner (Leaf 2) (Leaf 5))

instance (Show a) => Show (Tree a) where
  show (Leaf x)     = show x
  show (Inner lt rt) =  "<" ++ show lt ++ "|" ++ show rt ++ ">"

-- 65.1
instance (Eq a) => Eq (Tree a) where
    (==) (Leaf x) (Leaf y) = x == y
    (==) (Inner lx rx) (Inner ly ry) = (lx == ly) && (rx == ry)
    (==) _ _ = False
    (/=) tx ty = not (tx == ty)

-- 65.2
treeConcat :: Tree a -> Tree a -> Tree a
treeConcat t1 t2 = Inner t1 t2

-- 65.3
treeDepth :: (Num a, Ord a) => Tree b -> a
treeDepth (Leaf _) = 0
treeDepth (Inner lt rt) = (max (treeDepth lt) (treeDepth rt)) + 1

-- 65.4 TODO: wrap this with Maybe
treeB :: Int -> Tree Int
treeB 0 = Leaf 1
treeB n = if n < 0 then (treeB 0) else Inner (Leaf 0) (treeB (n-1))

-- 65.5
-- myfn :: (t -> Tree a) -> Tree 
myfn f (Leaf a) = f a
myfn f (Inner a b) = Inner (myfn f a) (myfn f b)

-- my own stuff
-- treeC :: (Ord a) => [a] -> Maybe (Tree b)
-- treeC :: (Ord a) => [a] -> Tree b
-- -- treeC _ = Nothing
-- treeC [] = Nothing
-- treeC [x] = Just (Leaf x)
-- treeC (x1:x2:xs) | x1 < x2 = Just (Inner x1 (treeC (x2:xs)))
--                  | otherwise = Just (Inner (treeC (x2:xs)) x1)
-- treeC [] = Leaf 0
-- treeC [x] = Leaf x
-- treeC (x1:x2:xs) | x1 < x2 = Inner x1 (treeC (x2:xs))
--                  | otherwise = Inner (treeC (x2:xs)) x1


-- Zadanie 66 TODO
data RF2 = RF2 Float Float deriving (Show, Eq)

-- 66.1
class VectorSpace a where
    vnull :: a
    vmult :: a -> Float -> a
    vadd :: a -> a -> a
    isBasis :: [a] -> Bool
    (<.>) :: a -> a -> Float

-- 66.2
-- instance (VectorSpace a) => VectorSpace (RF2 a) where
instance VectorSpace RF2 where
    vnull = RF2 0.0 0.0
    vmult (RF2 x y) t = RF2 (x*t) (y*t)
    vadd (RF2 x1 y1) (RF2 x2 y2) = RF2 (x1 + x2) (y1 + y2)
    isBasis vectors
       | length vectors /= 2 = False
       | otherwise =
           let calcDet ((RF2 x1 y1):(RF2 x2 y2):_) = x1 * y2 - x2 * y1
           in calcDet vectors /= 0
    (<.>) (RF2 x1 y1) (RF2 x2 y2) = x1 * x2 + y1 * y2

-- 66.5
data RF3 = RF3 {x :: Float, y :: Float, z :: Float} deriving (Show, Eq)

instance VectorSpace RF3 where
    vnull = RF3 0.0 0.0 0.0
    vmult v t = RF3 (x v * t) (y v * t) (z v * t)
    vadd v u = RF3 (x v + x u) (y v + y u) (z v * z u)
    isBasis vectors
        | length vectors /= 3 = False
        | otherwise = 
            let calcDet (v:u:w:_) = 0
            in calcDet vectors /= 0
    (<.>) v u = x v * x u + y v * y u + z v * z u


-- Zadanie 67
-- type Complex :: *

data Complex a = Complex !a !a  -- ! == strict field, czyli tylko jeden typ tam może się znaleźć.

-- instance (Num a) => Num (Complex a) where
--     Complex a b + Complex c d = Complex (a+c) (b+d)
--     Complex a b * Complex c d = Complex (a*b - b*d) (a*c + b*d)


-- Zadanie 68
data Prop = T |
            F |
            Var String |
            Or Prop Prop |
            And Prop Prop |
            Not Prop
            deriving Show

vars :: Prop -> [String]
vars p = mynub $ getvars p where
    mynub [] = []
    mynub (x:xs) = if x `elem` xs then mynub xs else x : (mynub xs)
    getvars T = []
    getvars F = []
    getvars (Var v) = [v]
    getvars (Not p) = getvars p
    getvars (Or p q) = (getvars p) ++ (getvars q)
    getvars (And p q) = (getvars p) ++ (getvars q)

-- data Error = EmptyList | NoEval

-- instance Show Error where
--     show EmptyList = "BŁĄD: Pusta Lista."
--     show NoEval = "BŁĄD: Brak ewaluacji zmiennej."

eval :: Prop -> [(String, Bool)] -> Bool
eval (Var p) xs = let getEval _ [] = False
                      getEval p (x:xs) = if p == (fst x) then snd x else getEval p xs
                  in getEval p xs
eval (Not p) xs = not (eval p xs)
eval (And p q) xs = (eval p xs) && (eval q xs)
eval (Or p q) xs = (eval p xs) || (eval q xs)
eval T _ = True
eval F _ = False

-- Napisz funkcj ̨e tautology:: Prop -> Bool, która sprawdza, czy dane zdanie jest taulologia.
tautology :: Prop -> Bool
tautology p = let myvars = vars p
                  allvars = [(map (\x -> (x, True)) myvars)] ++ [(map (\x -> (x, False)) myvars)]
              in all (\x -> eval p x) allvars

antitautology :: Prop -> Bool
antitautology p = let myvars = vars p
                      allvars = [map (\x -> (x, True)) myvars] ++ [map (\x -> (x, False)) myvars]
                  in all (\x -> not (eval p x)) allvars

simpl :: Prop -> Prop
simpl (Var p) = Var p
simpl T = T
simpl F = F
-- simpl (Not p) = simpl p -- ???
simpl (Not (Not p)) = simpl p
simpl (And T p) = simpl p
simpl (And p T) = simpl p
simpl (And F _) = F
simpl (And _ F) = F
simpl (Or T _) = T
simpl (Or _ T) = T
simpl (Or F p) = simpl p
simpl (Or p F) = simpl p
simpl (And (Not p) (Not q)) = simpl (Not (Or p q))
simpl (Or (Not p) (Not q)) = simpl (Not (And p q))
simpl p
    | tautology p = T
    -- | antitautology p = F
    | otherwise = p




