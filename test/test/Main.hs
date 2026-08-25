module Main where

import Control.Monad (unless)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Objective (MemoryUsage'))
import ILP.Compare (compareCost)
import ILP.Programs (TestProgram (..), programs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  oks <- mapM report programs
  unless (and oks) exitFailure
  where
    report p@(TestProgram name _) = do
      (old, new) <- compareCost MemoryUsage' p
      let ok = old == new
      putStrLn $ name ++ ": legacy=" ++ shown old ++ ", new=" ++ shown new ++ (if ok then " ok" else " mismatch")
      pure ok

    shown Nothing = "no solution"
    shown (Just c) = show c
