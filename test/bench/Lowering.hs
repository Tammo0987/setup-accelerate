{-# LANGUAGE TypeApplications #-}

module Lowering (suite) where

import Control.Exception.Base (evaluate)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (MIP), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), allVars, solve)
import qualified Data.Set as S
import Framework (Suite (..), Variant (..), median, timeIO)
import ILP.Programs (TestProgram (..), bytes, chain, radixsort, simple, twoFolds, twoSorts)
import ILP.Trafo (evalExpr, toFusionILP)
import ILP.Validate (rows)
import Text.Printf (printf)

data LoweringMode = Assign | Replace deriving (Show)

suite :: Suite
suite =
  Suite
    { suiteName = "lowering",
      suiteDescription = "Assignment constraints vs. variable replacement",
      suiteRuns = 3,
      suitePrograms =
        [ TestProgram "simple" simple,
          TestProgram "chain" chain,
          TestProgram "two-folds" twoFolds,
          TestProgram "sort-1" $ radixsort 1 bytes,
          TestProgram "sort-2" $ radixsort 2 bytes,
          TestProgram "two-sorts-1" $ twoSorts 1
        ],
      suiteVariants =
        [ Variant "assign" (benchProgram Assign),
          Variant "replace" (benchProgram Replace)
        ],
      suiteHeader = do
        printf
          "%-12s %-10s %-8s %-8s %-20s %-12s\n"
          "program"
          "mode"
          "#rows"
          "#vars"
          "median CBC [s]"
          "cost"
    }

benchProgram :: LoweringMode -> Int -> TestProgram -> IO ()
benchProgram mode runs (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ILPStages rawConstraints lowerAssign lowerReplace = makeILPStages MemoryUsage' fusedILP

  _ <- evaluate $ length rawConstraints

  problem <- case presolve rawConstraints of
    Left _ -> error "lowering: presolve found an infeasible problem"
    Right p -> pure p

  let (lower, lowerReference) = case mode of
        Assign -> (lowerAssign, lowerReplace)
        Replace -> (lowerReplace, lowerAssign)

      ilp@(ILP _ objective constraint _ constants) = lower problem

      referenceILP@(ILP _ referenceObjective _ _ referenceConstants) = lowerReference problem

      rowCount = length $ rows constraint
      variableCount = S.size $ allVars ilp

  _ <- evaluate rowCount
  _ <- evaluate variableCount

  _ <- solve (MIP cbc) ilp

  solveTimes <- mapM (const $ timeIO $ solve (MIP cbc) ilp) [1 .. runs]

  result <- solve (MIP cbc) ilp

  referenceResult <- solve (MIP cbc) referenceILP

  case (result, referenceResult) of
    (Just solution, Just referenceSolution) -> do
      let actualCost = evalExpr constants solution objective
          referenceCost = evalExpr referenceConstants referenceSolution referenceObjective

      if actualCost /= referenceCost
        then
          error $
            "lowering: cost mismatch for "
              ++ name
              ++ " in mode "
              ++ show mode
              ++ ": "
              ++ show actualCost
              ++ " /= "
              ++ show referenceCost
        else
          printf
            "%-12s %-10s %-8d %-8d %-20.6f %-12d\n"
            name
            (show mode)
            rowCount
            variableCount
            (median solveTimes)
            actualCost
    _ -> error $ "lowering: solver found no solution for " ++ name ++ " in mode " ++ show mode
