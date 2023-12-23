module Lattice where

class Lattice l where
  join :: l -> l -> l
  meet :: l -> l -> l

class Lattice l => BoundedBelow l where
  bottom :: l

class Lattice l => BoundedAbove l where
  top :: l
