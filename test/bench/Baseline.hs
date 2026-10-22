{-# LANGUAGE TypeApplications #-}

module Baseline (suite) where

import Control.Exception (evaluate)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (MIP), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), solve)
import Framework
import ILP.Programs (TestProgram (..), bytes, chain, radixsort, simple, twoFolds, twoSorts)
import ILP.Trafo (evalExpr, toFusionILP)
import Text.Printf (printf)

suite :: Suite
suite =
  Suite
    { suiteName = "baseline",
      suiteDescription = "Presolve followed by CBC solver",
      suiteRuns = 5,
      suitePrograms =
        [ TestProgram "simple" simple,
          TestProgram "chain" chain,
          TestProgram "twoFolds" twoFolds,
          TestProgram "sort-1" $ radixsort 1 bytes,
          TestProgram "sort-2" $ radixsort 2 bytes,
          TestProgram "two-sorts-1" $ twoSorts 1
        ],
      suiteVariants =
        [ Variant "cbc" benchProgram
        ],
      suiteHeader = do
        printf
          "%-12s %-20s %-20s %-12s\n"
          "program"
          "median presolve [s]"
          "median CBC [s]"
          "cost"
    }

benchProgram :: Int -> TestProgram -> IO ()
benchProgram runs (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ILPStages rawConstraints _ lower = makeILPStages MemoryUsage' fusedILP

      simplify constraints = case presolve constraints of
        Left _ -> error "baseline: presolve found an infeasible problem"
        Right problem -> problem

  _ <- evaluate $ length rawConstraints

  presolveTimes <- mapM (const $ timePure simplify rawConstraints) [1 .. runs]

  let problem = simplify rawConstraints
      ilp@(ILP _ objective _ _ constants) = lower problem

  _ <- solve (MIP cbc) ilp

  solveTimes <- mapM (const $ timeIO (solve (MIP cbc) ilp)) [1 .. runs]
  result <- solve (MIP cbc) ilp

  printf
    "%-12s %-20.6f %-20.6f %-12s\n"
    name
    (median presolveTimes)
    (median solveTimes)
    (maybe "infeasible" (show . (\solution -> evalExpr constants solution objective)) result)
