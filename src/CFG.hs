module CFG where

import Control.Monad.RWS
import Data.IntMap
import Data.Graph
import Syntax

type CFG = (Graph, Graph, IntMap Cmd)

type W = (IntMap Cmd, [Edge])
type S = Int

type GraphM = RWS () W S

addNode :: Cmd -> GraphM Vertex
addNode c =
  rws $ \_ v ->
    (v, v+1, (singleton v c, []))

addEdge :: Vertex -> Vertex -> GraphM ()
addEdge v1 v2 =
  rws $ \_ v ->
    ((), v, (empty, [(v1,v2)]))

true :: BExpr -> Cmd
true (Le x n) = CLe x n
true (Lt x n) = CLt x n
true (Ge x n) = CGe x n
true (Gt x n) = CGt x n

false :: BExpr -> Cmd
false (Le x n) = CGt x n
false (Lt x n) = CGe x n
false (Ge x n) = CLt x n
false (Gt x n) = CLe x n

makeCFG :: Stmt -> GraphM (Vertex, Vertex)
makeCFG Skip = do
  v <- addNode CSkip
  pure (v, v)
makeCFG (Assign x e) = do
  v <- addNode (CAssign x e)
  pure (v, v)
makeCFG (Seq s1 s2) = do
  (v1,v2) <- makeCFG s1
  (v3,v4) <- makeCFG s2
  addEdge v2 v3
  pure (v1, v4)
makeCFG (If b s1 s2) = do
  v0 <- addNode CSkip
  v1 <- addNode (true b)
  v2 <- addNode (false b)
  v3 <- addNode CSkip
  addEdge v0 v1
  addEdge v0 v2
  (i1, f1) <- makeCFG s1
  (i2, f2) <- makeCFG s2
  addEdge v1 i1
  addEdge v2 i2
  addEdge f1 v3
  addEdge f2 v3
  pure (v0, v3)
  where
makeCFG (While b s) = do
  v0 <- addNode CSkip
  v1 <- addNode (true b)
  v2 <- addNode (false b)
  addEdge v0 v1
  addEdge v0 v2
  (i,f) <- makeCFG s
  addEdge v1 i
  addEdge f v0
  pure (v0, v2)
makeCFG (Print e) = do
  v <- addNode (CPrint e)
  pure (v, v)

genCFG :: Stmt -> CFG
genCFG s = (g, g', m)
  where
    (_, n, (m, es)) = runRWS (makeCFG s) () 0
    g  = buildG (0,n-1) es
    g' = transposeG g
