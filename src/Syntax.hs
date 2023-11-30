module Syntax where

type Name = String
data AExpr
  = Var Name
  | ALit Integer
  | Add AExpr AExpr
  | Sub AExpr AExpr
  | Mul AExpr AExpr
  | Div AExpr AExpr
  deriving Show

data BExpr
  = Le Name Integer
  | Lt Name Integer
  | Ge Name Integer
  | Gt Name Integer
  deriving Show

data Stmt
  = Skip
  | Assign Name AExpr
  | Seq Stmt Stmt
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Print AExpr
  deriving Show

data Cmd
  = CSkip
  | CAssign Name AExpr
  | CLe Name Integer
  | CLt Name Integer
  | CGe Name Integer
  | CGt Name Integer
  | CPrint AExpr
  deriving Show

seqs :: [Stmt] -> Stmt
seqs = foldr Seq Skip
