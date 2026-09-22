{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (evaluate)
import Criterion.Measurement (initializeTime, measure)
import Criterion.Measurement.Types (Measured (measTime), whnf, whnfIO)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (Problem (..), emptySubstitution, presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), allVars, solve)
import Data.Either (fromRight)
import Data.List (sort)
import qualified Data.Set as S
import ILP.Programs (TestProgram (..), bytes, radixsort, twoSorts)
import ILP.Trafo (toFusionILP)
import ILP.Validate (rows)
import Text.Printf (printf)

objectives :: [Objective]
objectives = [MemoryUsage']

runs :: Int
runs = 2

benchPrograms :: [TestProgram]
benchPrograms =
  [ TestProgram "sort-1" $ radixsort 1 bytes,
    TestProgram "sort-2" $ radixsort 2 bytes,
    TestProgram "sort-4" $ radixsort 4 bytes,
    TestProgram "sort-8" $ radixsort 8 bytes,
    TestProgram "twoSorts-1" $ twoSorts 1,
    TestProgram "twoSorts-2" $ twoSorts 2,
    TestProgram "twoSorts-4" $ twoSorts 4
  ]

main :: IO ()
main = do
  initializeTime
  printf
    "%-12s %-9s %-6s %-6s %-20s %-16s\n"
    "program"
    "presolve"
    "rows"
    "cols"
    "median presolve [s]"
    "median solve [s]"
  mapM_ benchProgram benchPrograms

benchProgram :: TestProgram -> IO ()
benchProgram (TestProgram name f) =
  sequence_
    [ run usePresolve objective
    | objective <- objectives,
      usePresolve <- [False, True]
    ]
  where
    fusedILP = toFusionILP @NativeOp defaultOptions f

    run usePresolve objective = do
      let ILPStages rawConstraints lower = makeILPStages objective fusedILP

      -- Generate the initial constraint-list spine before timing presolve.
      _ <- evaluate $ length rawConstraints

      presolveTimes <-
        if usePresolve
          then mapM (const $ timePure presolve rawConstraints) [1 .. runs]
          else pure []

      let problem
            | usePresolve =
                fromRight
                  (error "presolve: ILP is infeasible")
                  (presolve rawConstraints)
            | otherwise =
                Problem rawConstraints emptySubstitution

          ilp@(ILP _ _ constraint _ _) = lower problem

      -- Warm up the solver and force the lowered ILP.
      _ <- solve (MIP cbc) ilp

      solveTimes <- mapM (const $ timeIO $ solve (MIP cbc) ilp) [1 .. runs]

      printf
        "%-12s %-9s %-6d %-6d %-20s %-16.6f\n"
        name
        (show usePresolve)
        (length $ rows constraint)
        (S.size $ allVars ilp)
        ( case presolveTimes of
            [] -> "-"
            times -> printf "%.6f" (median times)
        )
        (median solveTimes)

    timePure :: (a -> b) -> a -> IO Double
    timePure function input = measTime . fst <$> measure (whnf function input) 1

    timeIO :: IO a -> IO Double
    timeIO action = measTime . fst <$> measure (whnfIO action) 1

median :: [Double] -> Double
median xs = sort xs !! (length xs `div` 2)
