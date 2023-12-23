{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Interval where

import GHC.Natural
import Lattice

data Interval
  = Bottom
  | Top
  | UpperBound Integer
  | LowerBound Integer
  | FinRange Integer Natural
  deriving (Eq, Show)

view :: Interval -> Maybe (Bot Integer, Top Integer)
view Bottom = Nothing
view Top    = Just (BotElem, TopElem)
view (UpperBound n) = Just (BotElem, NotTop n)
view (LowerBound n) = Just (NotBot n, TopElem)
view (FinRange m n) = Just (NotBot m, NotTop (m + fromIntegral n))

pattern Range :: Bot Integer -> Top Integer -> Interval
pattern Range a b <- (view -> Just (a, b)) where
  Range BotElem    TopElem    = Top
  Range BotElem    (NotTop b) = UpperBound b
  Range (NotBot a) TopElem    = LowerBound a
  Range (NotBot a) (NotTop b) | a <= b    = FinRange a (fromIntegral (b - a))
                              | otherwise = Bottom

{-# COMPLETE Bottom, Range #-}

instance Lattice Interval where
  join Bottom i2 = i2
  join i1 Bottom = i1
  join (Range a1 b1) (Range a2 b2) = Range (min a1 a2) (max b1 b2)
  meet Bottom _ = Bottom
  meet _ Bottom = Bottom
  meet (Range a1 b1) (Range a2 b2) = Range (max a1 a2) (min b1 b2)

instance BoundedBelow Interval where
  bottom = Bottom

instance BoundedAbove Interval where
  top = Top

instance Widening Interval where
  wide Bottom i2 = i2
  wide i1 Bottom = i1
  wide (Range a b) (Range c d) = Range l u
    where
      l = if c < a then BotElem else a
      u = if b < d then TopElem else b

instance Narrowing Interval where
  narrow Bottom _ = Bottom
  narrow _ Bottom = Bottom
  narrow (Range a b) (Range c d) = Range l u
    where
      l = if a == BotElem then c else a
      u = if b == TopElem then d else b

iadd :: Interval -> Interval -> Interval
iadd Bottom _ = Bottom
iadd _ Bottom = Bottom
iadd (Range a1 b1) (Range a2 b2) = Range l u
  where
    l = case (a1, a2) of
          (BotElem, _) -> BotElem
          (_, BotElem) -> BotElem
          (NotBot x, NotBot y) -> NotBot (x+y)
    u = case (b1, b2) of
          (TopElem, _) -> TopElem
          (_, TopElem) -> TopElem
          (NotTop x, NotTop y) -> NotTop (x+y)

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
