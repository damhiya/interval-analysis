module Analysis where

import Data.Array  qualified as A
import Data.IntMap qualified as IM
import CFG
import Lattice
import Interval
import Domain
import Syntax
import Apply

eval :: State -> AExpr -> Interval
eval rho (Var x) = apply rho x
eval _   (ALit n) = FinRange n 0
eval rho (Add e1 e2) = eval rho e1 `iadd` eval rho e2
eval rho (Sub e1 e2) = eval rho e1 `isub` eval rho e2
eval rho (Mul e1 e2) = eval rho e1 `imul` eval rho e2
eval rho (Div e1 e2) = eval rho e1 `idiv` eval rho e2

step :: Cmd -> State -> State
step CSkip s = s
step (CAssign x e) s = adjust x (\_ -> eval s e) s
step (CLe x n) s = adjust x (\i -> meet i (UpperBound n))     s
step (CLt x n) s = adjust x (\i -> meet i (UpperBound (n-1))) s
step (CGe x n) s = adjust x (\i -> meet i (LowerBound n))     s
step (CGt x n) s = adjust x (\i -> meet i (LowerBound (n+1))) s
step (CPrint _) s = s

step' :: CFG -> Domain -> Domain
step' (g, g', cs) d = Domain (IM.mapWithKey go cs)
  where
    go :: Int -> Cmd -> State
    go v c = step c s
      where
        ss = apply d <$> (g' A.! v)
        s = joinAll ss

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
