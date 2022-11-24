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
    -- isBasis :: [a] -> Bool
    (<.>) :: a -> a -> Float

-- 66.2
-- instance (VectorSpace a) => VectorSpace (RF2 a) where
instance VectorSpace RF2 where
    vnull = RF2 0.0 0.0
    vmult (RF2 x y) t = RF2 (x*t) (y*t)
    vadd (RF2 x1 y1) (RF2 x2 y2) = RF2 (x1 + x2) (y1 + y2)
    -- isBasis vectors
    --    | length vectors /= 2 = False
    --    | otherwise =
    --        let calcDet (x:y:z:_) = 
    --        in calcDet vectors /= 0
    (<.>) (RF2 x1 y1) (RF2 x2 y2) = x1 * x2 + y1 * y2

-- 66.5
data RF3 = RF3 {x :: Float, y :: Float, z :: Float} deriving (Show, Eq)

instance VectorSpace RF3 where
    vnull = RF3 0.0 0.0 0.0
    vmult v t = RF3 (x v * t) (y v * t) (z v * t)
    vadd v u = RF3 (x v + x u) (y v + y u) (z v * z u)
    (<.>) v u = x v * x u + y v * y u + z v * z u


-- Zadanie 67
-- type Complex :: *

data Complex a = Complex !a !a  -- ! == strict field, czyli tylko jeden typ tam może się znaleźć.

-- instance (Num a) => Num (Complex a) where
--     Complex a b + Complex c d = Complex (a+c) (b+d)
--     Complex a b * Complex c d = Complex (a*b - b*d) (a*c + b*d)


-- Zadanie 68
-- data Prop = T |
--             F |
--             (Prop or Prop) |
--             (Prop and Prop) |
--             (no Prop)

