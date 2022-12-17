import Control.Monad.State

-- UWAGA: korzystamy z haskelowej implementacji
-- Tutaj przypomnienie postawowych funkcji
 
-- get :: State s s
-- get = λs → (s, s)

-- put :: s → State s ()
-- put s = λ_ → ((), s)

-- return :: a -> State s a
-- return x = (λs - > (x,s)

data Tree t = Empty | Node (Tree t) t (Tree t) deriving (Eq)
instance Show a => Show (Tree a) where
  show Empty = "*"
  show (Node lt a rt) = "["++show lt ++ "("++ show a ++")" ++ show rt ++ "]"

treeA = Node Empty 'a' Empty
treeB = Node Empty 'c' treeA
treeC = Node Empty 'd' treeB
treeD = Node Empty 'e' treeC

relabel :: Tree a -> State Int (Tree Int)
relabel Empty = return Empty
relabel (Node left _ right) = do
  lt  <- relabel left 
  lab <- get   
  put (lab+1)
  rt  <- relabel right
  return (Node lt (lab+1) rt)

treeDepth :: Tree a -> State Int ()
treeDepth Empty = put 0
treeDepth (Node left _ right) = do
  treeDepth left
  n <- get
  put 0       -- resetujemy licznik 
  treeDepth right
  m <- get 
  put ((max n m) + 1)
    
    