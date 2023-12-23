module Interval where

import GHC.Natural
import Lattice

data EInteger = NInf | Fin Integer | PInf
  deriving (Eq, Ord, Show)

data Interval
  = Bottom
  | Top
  | UpperBound Integer
  | LowerBound Integer
  | FinRange Integer Natural
  deriving (Eq, Show)

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

instance Lattice Interval where
  join i1 i2 =
    case (iminmax i1, iminmax i2) of
      (Nothing, _) -> i2
      (_, Nothing) -> i1
      (Just (a1, b1), Just (a2, b2)) ->
        erange (min a1 a2) (max b1 b2)
  meet i1 i2 =
    case (iminmax i1, iminmax i2) of
      (Nothing, _) -> Bottom
      (_, Nothing) -> Bottom
      (Just (a1, b1), Just (a2, b2)) ->
        erange (max a1 a2) (min b1 b2)

instance BoundedBelow Interval where
  bottom = Bottom

instance BoundedAbove Interval where
  top = Top

instance Widening Interval where
  wide i1 i2 =
    case (iminmax i1, iminmax i2) of
      (Nothing, _) -> i2
      (_, Nothing) -> i1
      (Just (a, b), Just (c, d)) ->
        erange (if c < a then NInf else a) (if b < d then PInf else b)

instance Narrowing Interval where
  narrow i1 i2 =
    case (iminmax i1, iminmax i2) of
      (Nothing, _) -> Bottom
      (_, Nothing) -> Bottom
      (Just (a, b), Just (c, d)) ->
        erange (if a == NInf then c else a) (if b == PInf then d else b)

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
