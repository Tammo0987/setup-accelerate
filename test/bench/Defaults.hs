{-# LANGUAGE TypeApplications #-}

module Defaults (suite) where

import Control.Exception (evaluate)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.BinaryDefaults (applyBinaryDefaults, defaultBinaryAssignments)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.CustomSolver (CompletionError, FeasibleSolution (..), solveOptimal, solveOptimalWithBinaryDefaults, solveOptimalUsing)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.DirectionDefaults (applyDirectionDefaults)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (Problem (..), emptySubstitution, presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), allVars, solve)
import qualified Data.Map as M
import Framework (Suite (..), Variant (..), median, timePure)
import ILP.Programs (TestProgram (..), bytes, chain, radixsort, simple)
import ILP.Trafo (evalExpr, toFusionILP)
import Text.Printf (printf)

data DefaultsMode = Disabled | Binary | Direction | Both
  deriving (Show)

suite :: Suite
suite =
  Suite
    { suiteName = "defaults",
      suiteDescription = "Optional binary and direction defaults in the custom solver",
      suiteRuns = 3,
      suitePrograms =
        [ TestProgram "simple" simple,
          TestProgram "chain" chain,
          TestProgram "sort-1" $ radixsort 1 bytes,
          TestProgram "sort-2" $ radixsort 2 bytes
        ],
      suiteVariants =
        [ Variant "disabled" (benchProgram Disabled),
          Variant "binary" (benchProgram Binary),
          Variant "direction" (benchProgram Direction),
          Variant "both" (benchProgram Both)
        ],
      suiteHeader =
        printf
          "%-12s %-10s %-20s %-20s %-12s %-12s\n"
          "program"
          "defaults"
          "median presolve [s]"
          "median custom [s]"
          "custom cost"
          "CBC cost"
    }

benchProgram :: DefaultsMode -> Int -> TestProgram -> IO ()
benchProgram mode runs (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ILPStages raw lowerAssignments lower = makeILPStages MemoryUsage' fusedILP
      variables = allVars $ lowerAssignments (Problem raw emptySubstitution)
      solver = case mode of
        Disabled -> solveOptimal lower variables
        Binary -> solveOptimalWithBinaryDefaults lower variables
        Direction -> solveOptimalUsing (applyDirectionDefaults lower) lower variables
        Both -> solveOptimalUsing (applyBothDefaults lower) lower variables
      simplify input = case presolve input of
        Left _ -> error "defaults: infeasible presolve"
        Right problem -> problem

  _ <- evaluate $ length raw
  presolveTimes <- mapM (const $ timePure simplify raw) [1 .. runs]

  let problem = simplify raw
      ilp@(ILP _ objective _ _ constants) = lower problem

  _ <- evaluate problem
  solverTimes <- mapM (const $ timePure (solverChecksum . solver) problem) [1 .. runs]

  -- Check the original optimum, not the possibly restricted defaulted problem.
  reference <- solve (MIP cbc) ilp
  case (solver problem, reference) of
    (Left failure, _) -> error $ "defaults: " ++ show failure
    (_, Nothing) -> error "defaults: CBC found no solution"
    (Right optimal, Just cbcSolution) -> do
      let referenceCost = evalExpr constants cbcSolution objective
      if cost optimal /= referenceCost
        then
          error $
            "defaults: cost mismatch for "
              ++ name
              ++ " in mode "
              ++ show mode
              ++ ": "
              ++ show (cost optimal)
              ++ " /= "
              ++ show referenceCost
        else
          printf
            "%-12s %-10s %-20.6f %-20.6f %-12d %-12d\n"
            name
            (show mode)
            (median presolveTimes)
            (median solverTimes)
            (cost optimal)
            referenceCost

applyBothDefaults :: (Problem -> ILP) -> Problem -> Maybe Problem
applyBothDefaults lower problem = do
  directed <- applyDirectionDefaults lower problem
  if M.null (defaultBinaryAssignments lower directed)
    then Just directed
    else applyBinaryDefaults lower directed >>= applyBothDefaults lower

solverChecksum :: Either CompletionError FeasibleSolution -> Int
solverChecksum (Left failure) = error $ "defaults: " ++ show failure
solverChecksum (Right feasible) = cost feasible + M.size (solution feasible)
