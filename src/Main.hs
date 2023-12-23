module Main where

import Examples
import CFG
import Analysis

main :: IO ()
main = do
  putStrLn "sum - 0"
  print (analysis 0   . genCFG $ exsum)
  putStrLn "sum - 100"
  print (analysis 100 . genCFG $ exsum)
  putStrLn "count - 0"
  print (analysis 0   . genCFG $ excount)



