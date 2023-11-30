module Domain where

import Data.Map as M
import Data.IntMap as IM
import GHC.Natural
import Syntax

data EInteger = NInf | Fin Integer | PInf
  deriving (Eq, Ord)

data Interval
  = Bottom
  | Top
  | UpperBound Integer
  | LowerBound Integer
  | FinRange Integer Natural

-- inf, sup :: Interval -> EInteger
-- inf Bottom         = PInf
-- inf Top            = NInf
-- inf (UpperBound n) = NInf
-- inf (LowerBound n) = Fin n
-- inf (FinRange n _) = Fin n
-- sup Bottom         = NInf
-- sup Top            = PInf
-- sup (UpperBound n) = Fin n
-- sup (LowerBound n) = PInf
-- sup (FinRange n m) = Fin (n + fromIntegral m)

iminmax :: Interval -> Maybe (EInteger, EInteger)
iminmax Bottom = Nothing
iminmax Top = Just (NInf, PInf)
iminmax (UpperBound n) = Just (NInf, Fin n)
iminmax (LowerBound n) = Just (Fin n, PInf)
iminmax (FinRange n m) = Just (Fin n, Fin (n + fromIntegral m))

iadd :: Interval -> Interval -> Interval
iadd i1 i2 =
  case (iminmax i1, iminmax i2) of
    (Nothing, _) -> Bottom
    (_, Nothing) -> Bottom
    (Just (a1, b1), Just (a2, b2)) ->
      case (min a1 a2, max b1 b2) of
        (NInf, PInf) -> Top
        (NInf, Fin n) -> UpperBound n
        (Fin n, PInf) -> LowerBound n
        (Fin m, Fin n) -> FinRange m (fromIntegral (n - m))
        (PInf, _) -> Bottom -- unreachable
        (_, NInf) -> Bottom -- unreachable

isub :: Interval -> Interval -> Interval
isub = undefined
imul :: Interval -> Interval -> Interval
imul = undefined
idiv :: Interval -> Interval -> Interval
idiv = undefined

type State = Map Name Interval
type Domain = IntMap State

eval :: State -> AExpr -> Interval
eval rho (Var x) = case M.lookup x rho of
                     Nothing -> Top
                     Just i  -> i
eval _   (ALit n) = FinRange n 0
eval rho (Add e1 e2) = eval rho e1 `iadd` eval rho e2
eval rho (Sub e1 e2) = eval rho e1 `isub` eval rho e2
eval rho (Mul e1 e2) = eval rho e1 `imul` eval rho e2
eval rho (Div e1 e2) = eval rho e1 `idiv` eval rho e2
