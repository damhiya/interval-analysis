module Lattice where

class Lattice l where
  join :: l -> l -> l
  meet :: l -> l -> l

class Lattice l => BoundedBelow l where
  bottom :: l

class Lattice l => BoundedAbove l where
  top :: l

class Lattice l => Widening l where
  wide :: l -> l -> l

class Lattice l => Narrowing l where
  narrow :: l -> l -> l

joinAll :: (Lattice l, BoundedBelow l) => [l] -> l
joinAll = foldr join bottom

meetAll :: (Lattice l, BoundedAbove l) => [l] -> l
meetAll = foldr meet top
