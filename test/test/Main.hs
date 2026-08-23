module Main where

import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Objective (MemoryUsage'))
import ILP.Compare (compareCost)
import ILP.Programs (TestProgram (..), programs)

main :: IO ()
main = mapM_ report programs
  where
    report p@(TestProgram name _) = do
      (old, new) <- compareCost MemoryUsage' p
      putStrLn $ name ++ ": legacy=" ++ shown old ++ ", new=" ++ shown new ++ (if old == new then " ok" else " mismatch")

    shown Nothing = "no solution"
    shown (Just c) = show c
