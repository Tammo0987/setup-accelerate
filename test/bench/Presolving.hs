{-# LANGUAGE TypeApplications #-}

module Presolving (suite) where

import Control.Exception (evaluate)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (MIP), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (Problem (..), emptySubstitution, presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), solve)
import Data.IORef (newIORef, readIORef)
import Framework (Suite (..), Variant (..), median, timeIO)
import ILP.Programs (TestProgram (..), bytes, chain, radixsort, simple, twoFolds, twoSorts)
import ILP.Trafo (evalExpr, toFusionILP)
import Text.Printf (printf)

suite :: Suite
suite =
  Suite
    { suiteName = "presolving",
      suiteDescription = "CBC alone vs. custom presolving followed by CBC",
      suiteRuns = 5,
      suitePrograms =
        [ TestProgram "simple" simple,
          TestProgram "chain" chain,
          TestProgram "twoFolds" twoFolds,
          TestProgram "sort-1" $ radixsort 1 bytes,
          TestProgram "sort-2" $ radixsort 2 bytes,
          TestProgram "sort-3" $ radixsort 3 bytes,
          TestProgram "sort-4" $ radixsort 4 bytes,
          TestProgram "two-sorts-1" $ twoSorts 1,
          TestProgram "two-sorts-2" $ twoSorts 2,
          TestProgram "two-sorts-3" $ twoSorts 3
        ],
      suiteVariants = [Variant "comparison" benchProgram],
      suiteHeader = do
        putStrLn "Compare CBC alone vs. presolver + CBC"
        printf
          "%-12s %-20s %-25s %-12s\n"
          "program"
          "median CBC [s]"
          "median presolve + CBC [s]"
          "cost"
    }

benchProgram :: Int -> TestProgram -> IO ()
benchProgram runs (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ILPStages rawConstraints _ lower = makeILPStages MemoryUsage' fusedILP
      simplify constraints = case presolve constraints of
        Left _ -> error $ "presolving: presolve found an infeasible problem for " ++ name
        Right problem -> problem

  _ <- evaluate $ length rawConstraints
  input <- newIORef rawConstraints

  let run usePresolve = do
        constraints <- readIORef input
        let problem
              | usePresolve = simplify constraints
              | otherwise = Problem constraints emptySubstitution
            ilp@(ILP _ objective _ _ constants) = lower problem
        result <- solve (MIP cbc) ilp
        case result of
          Nothing -> error $ "presolving: CBC found no solution for " ++ name ++ " (presolve enabled: " ++ show usePresolve ++ ")"
          Just solution -> evaluate $ evalExpr constants solution objective

  _ <- run False
  _ <- run True

  withoutTimes <- mapM (const $ timeIO $ run False) [1 .. runs]
  withTimes <- mapM (const $ timeIO $ run True) [1 .. runs]

  rawCost <- run False
  presolvedCost <- run True
  if rawCost /= presolvedCost
    then
      error $
        "presolving: cost mismatch for " ++ name
          ++ ": without presolve " ++ show rawCost
          ++ ", with presolve " ++ show presolvedCost
    else
      printf
        "%-12s %-20.6f %-25.6f %-12d\n"
        name
        (median withoutTimes)
        (median withTimes)
        rawCost
