module Analysis where

import Data.Array  qualified as A
import Data.IntMap qualified as IM
import Data.Graph
import CFG
import Lattice
import Interval
import Domain
import Syntax
import Apply

step :: Cmd -> State -> State
step CSkip s = s
step (CAssign x e) s = adjust x (\_ -> eval s e) s
step (CLe x n) s = adjust x (\i -> meet i (UpperBound n))     s
step (CLt x n) s = adjust x (\i -> meet i (UpperBound (n-1))) s
step (CGe x n) s = adjust x (\i -> meet i (LowerBound n))     s
step (CGt x n) s = adjust x (\i -> meet i (LowerBound (n+1))) s
step (CPrint _) s = s

step' :: CFG -> Domain -> Domain
step' (g, cs) d = Domain (IM.mapWithKey go cs)
  where
    g' = transposeG g

    go :: Int -> Cmd -> State
    go n c = step c s
      where
        ns = g' A.! n
        s = joinAll (map (apply d) ns)

fpow :: Int -> (a -> a) -> (a -> a)
fpow n f 
  | n <= 0    = id
  | otherwise = f . fpow (n-1) f

ifix :: Eq a => (a -> a) -> a -> a
ifix f x = if x == x' then x else ifix f x'
  where
    x' = f x

analysis :: Int -> CFG -> Domain
analysis n cfg = (ifix fn . ifix fw . fpow n f) bottom
  where
    f  x = step' cfg x
    fw x = wide   x (f x)
    fn x = narrow x (f x)
