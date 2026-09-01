module Main where

import Control.Monad (unless)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Objective (..))
import ILP.Compare (compareCost)
import ILP.Programs (TestProgram (..), programs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  oks <- sequence [report obj p | obj <- objectives, p <- programs]
  unless (and oks) exitFailure
  where
    objectives = [NumClusters, ArrayReads, ArrayReadsWrites, IntermediateArrays, FusedEdges, Everything , ArrayReads', ArrayReadsWrites', IntermediateArrays', FusedEdges', MemoryUsage, MemoryUsage']

    report obj p@(TestProgram name _) = do
      (old, new) <- compareCost obj p
      let ok = old == new
      putStrLn $ show obj ++ " / " ++ name ++ ": legacy=" ++ shown old ++ ", new=" ++ shown new ++ (if ok then " ok" else " mismatch")
      pure ok

    shown Nothing = "no solution"
    shown (Just c) = show c
