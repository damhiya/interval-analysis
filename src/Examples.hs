module Examples where

import Syntax

exsum :: Stmt
exsum =
  seqs
    [ Assign x (ALit 10)
    , Assign y (ALit 0)
    , While (Ge x 0) $ seqs
      [ Assign y (Add (Var y) (Var x))
      , Assign x (Sub (Var x) (ALit 1))
      ]
    ]
  where
    x = "x"
    y = "y"

excount :: Stmt
excount =
  seqs
    [ Assign x (ALit 0)
    , Assign y (ALit 0)
    , While (Lt x 10) $ seqs
      [ Assign x (Add (Var x) (ALit 1))
      , Assign y (Add (Var y) (ALit 1))
      ]
    , Print (Var x)
    ]
  where
    x = "x"
    y = "y"
