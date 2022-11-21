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
treeB 0 = Inner (Leaf 0) (Leaf 1)
treeB n = if n < 0 then (treeB 0) else Inner (Leaf 0) (treeB (n-1))

-- 65.5
-- ...


-- Zadanie 66 TODO
data RF2 a = RF2 Float Float deriving (Show, Eq)

-- 66.1
class VectorSpace a where
    vnull :: (VectorSpace a) => a
    -- vmult :: (VectorSpace a, Num b) => a -> a -> b
    vadd :: (VectorSpace a) => a -> a -> a

-- 66.2
instance (VectorSpace a) => VectorSpace (RF2 a) where
    vnull = RF2 0.0 0.0
    -- vmult (RF2 x1 x2) (RF2 y1 y2) = x1*y1 + x2*y2
    vadd (RF2 x1 x2) (RF2 y1 y2) = RF2 (x1 + y1) (x2 + y2)

