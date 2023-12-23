module Domain where

import Data.Map as M
import Data.Map.Merge.Lazy as M
import Data.IntMap as IM
import Data.IntMap.Merge.Lazy as IM
import Lattice
import Interval
import Syntax

newtype State = State (Map Name Interval)
newtype Domain = Domain (IntMap State)

instance Lattice State where
  join (State s1) (State s2) = State (m s1 s2)
    where
      m = M.merge
            M.preserveMissing
            M.preserveMissing
            (M.zipWithMatched (\_ -> join))

  meet (State s1) (State s2) = State (m s1 s2)
    where
      m = M.merge
            M.dropMissing
            M.dropMissing
            (M.zipWithMatched (\_ -> meet))

instance BoundedBelow State where
  bottom = State M.empty

instance Lattice Domain where
  join (Domain d1) (Domain d2) = Domain (m d1 d2)
    where
      m = IM.merge
            IM.preserveMissing
            IM.preserveMissing
            (IM.zipWithMatched (\_ -> join))

  meet (Domain d1) (Domain d2) = Domain (m d1 d2)
    where
      m = IM.merge
            IM.dropMissing
            IM.dropMissing
            (IM.zipWithMatched (\_ -> meet))

instance BoundedBelow Domain where
  bottom = Domain IM.empty

apply :: State -> Name -> Interval
apply (State rho) x = case M.lookup x rho of
                        Nothing -> Bottom
                        Just i  -> i

eval :: State -> AExpr -> Interval
eval rho (Var x) = apply rho x
eval _   (ALit n) = FinRange n 0
eval rho (Add e1 e2) = eval rho e1 `iadd` eval rho e2
eval rho (Sub e1 e2) = eval rho e1 `isub` eval rho e2
eval rho (Mul e1 e2) = eval rho e1 `imul` eval rho e2
eval rho (Div e1 e2) = eval rho e1 `idiv` eval rho e2
