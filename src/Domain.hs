module Domain where

import Data.Map as M
import Data.IntMap as IM
import GHC.Natural
import Syntax

data EInteger = NInf | Fin Integer | PInf
  deriving (Eq, Ord, Show)

data Interval
  = Bottom
  | Top
  | UpperBound Integer
  | LowerBound Integer
  | FinRange Integer Natural
  deriving Show

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

range :: Integer -> Integer -> Interval
range m n | m <= n    = FinRange m (fromIntegral (n - m))
          | otherwise = Bottom

erange :: EInteger -> EInteger -> Interval
erange NInf    PInf    = Top
erange NInf    (Fin n) = UpperBound n
erange (Fin n) PInf    = LowerBound n
erange (Fin m) (Fin n) | m <= n    = FinRange m (fromIntegral (n - m))
                       | otherwise = Bottom
erange _ _ = Bottom

join :: Interval -> Interval -> Interval
join i1 i2 =
  case (iminmax i1, iminmax i2) of
    (Nothing, _) -> i2
    (_, Nothing) -> i1
    (Just (a1, b1), Just (a2, b2)) ->
      erange (min a1 a2) (max b1 b2)

meet :: Interval -> Interval -> Interval
meet i1 i2 =
  case (iminmax i1, iminmax i2) of
    (Nothing, _) -> Bottom
    (_, Nothing) -> Bottom
    (Just (a1, b1), Just (a2, b2)) ->
      erange (max a1 a2) (min b1 b2)

iadd :: Interval -> Interval -> Interval
iadd i1 i2 =
  case (iminmax i1, iminmax i2) of
    (Nothing, _) -> Bottom
    (_, Nothing) -> Bottom
    (Just (a1, b1), Just (a2, b2)) ->
      let l =
            case (a1, a2) of
              (NInf, _) -> NInf
              (_, NInf) -> NInf
              (Fin x, Fin y) -> Fin (x+y)
              _ -> undefined
          u =
            case (b1, b2) of
              (PInf, _) -> PInf
              (_, PInf) -> PInf
              (Fin x, Fin y) -> Fin (x+y)
              _ -> undefined
      in erange l u

ineg :: Interval -> Interval
ineg Bottom = Bottom
ineg Top = Top
ineg (UpperBound n) = LowerBound (-n)
ineg (LowerBound n) = UpperBound (-n)
ineg (FinRange m n) = FinRange (-(m + fromIntegral n)) n

isub :: Interval -> Interval -> Interval
isub i1 i2 = iadd i1 (ineg i2)

imul :: Interval -> Interval -> Interval
imul = undefined
idiv :: Interval -> Interval -> Interval
idiv = undefined

wide :: Interval -> Interval -> Interval
wide i1 i2 =
  case (iminmax i1, iminmax i2) of
    (Nothing, _) -> i2
    (_, Nothing) -> i1
    (Just (a, b), Just (c, d)) ->
      erange (if c < a then NInf else a) (if b < d then PInf else b)

narrow :: Interval -> Interval -> Interval
narrow i1 i2 =
  case (iminmax i1, iminmax i2) of
    (Nothing, _) -> Bottom
    (_, Nothing) -> Bottom
    (Just (a, b), Just (c, d)) ->
      erange (if a == NInf then c else a) (if b == PInf then d else b)

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
