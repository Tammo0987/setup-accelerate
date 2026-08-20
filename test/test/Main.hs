module Main where

import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Objective (MemoryUsage'))
import ILP.Compare (legacyCost)
import qualified ILP.ConstraintLanguage as CL
import ILP.Programs (TestProgram (..), programs)

main :: IO ()
main = mapM_ report programs
  where
    report p@(TestProgram name _) = do
      cost <- legacyCost MemoryUsage' p
      putStrLn $ name ++ ": " ++ maybe "no solution" show cost
