module Analysis where

import Data.Array  qualified as A
import Data.Map    qualified as M
import Data.IntMap qualified as IM
import Data.Graph
import CFG
import Domain
import Syntax

step :: Cmd -> State -> State
step CSkip s = s
step (CAssign x e) s = M.insert x (eval s e) s
step (CLe x n) s = M.adjust (\i -> meet i (UpperBound n)) x s
step (CLt x n) s = M.adjust (\i -> meet i (UpperBound (n-1))) x s
step (CGe x n) s = M.adjust (\i -> meet i (LowerBound n)) x s
step (CGt x n) s = M.adjust (\i -> meet i (LowerBound (n+1))) x s
step (CPrint _) s = s

step' :: CFG -> Domain -> Domain
step' (g, cs) d = IM.mapWithKey go cs
  where
    g' = transposeG g

    go :: Int -> Cmd -> State
    go n c = undefined
      where
        ns = g' A.! n
        -- s = foldr join ()
