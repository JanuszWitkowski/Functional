import Data.List

data Graph = Graph [Int] [(Int, Int)] deriving Show

addVertices :: Graph -> [Int] -> Graph
addVertices (Graph vs1 es) vs2 = Graph (nub (vs1 ++ vs2)) es

addEdges :: Graph -> [(Int, Int)] -> Graph
addEdges (Graph vs es1) es2 = Graph vs (nub (es1 ++ es2))

addToGraph :: Graph -> [Int] -> [(Int, Int)] -> Graph
addToGraph (Graph vs1 es1) vs2 es2 = Graph (nub (vs1 ++ vs2)) (nub (es1 ++ es2))

mergeGraphs :: Graph -> Graph -> Graph
mergeGraphs g1 (Graph vs2 es2) = addToGraph g1 vs2 es2


data MGraph a = MGraph [a] [(a,a)] deriving Show

instance Functor MGraph where
    fmap f (MGraph vs es) = MGraph (map f vs) (map (\(x,y) -> (f x, f y)) es)

instance Applicative MGraph where
    pure x = MGraph [x] []
    (MGraph fv fe) <*> (MGraph vs es) = MGraph (map (head fv) vs) (map (\(a,b) -> ((fst hfe) a, (snd hfe) b)) es) where hfe = head fe

instance Monad MGraph where
    return = pure
    (MGraph vs es) >>= f = f (head vs)


-- From OpenAI Chat
data MonadicGraph a = MonadicGraph [a] [(a,a)] deriving (Show, Eq)

instance Functor MonadicGraph where
    fmap f (MonadicGraph vs es) = MonadicGraph (map f vs) (map (\(x,y) -> (f x, f y)) es)

-- instance Applicative MonadicGraph where
--     pure x = MonadicGraph [x] []
--     -- ...

-- instance Monad MonadicGraph where
--     return x = MonadicGraph [x] []
--     (MonadicGraph xs ys) >>= f = MonadicGraph zs ws
--         where zs = nub $ concatMap (\(MonadicGraph xs' ys') -> xs') (map f xs)
--               ws = concatMap (\(MonadicGraph xs' ys') -> ys') (map f xs)


