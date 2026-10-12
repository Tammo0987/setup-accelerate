{-# LANGUAGE TypeApplications #-}

module Splitting (suite) where

import Control.Exception (evaluate)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Components (componentSizes, solveOptimalWithSplitWithReport)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.CustomSolver (CompletionError, FeasibleSolution (..), solveOptimal)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (Problem (..), emptySubstitution, presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), allVars, solve)

import Data.List (sort)
import qualified Data.Map as M
import Framework (Suite (..), Variant (..), median, timePure)
import ILP.Programs (TestProgram (..), bytes, chain, radixsort, simple)
import ILP.Trafo (evalExpr, toFusionILP)
import Text.Printf (printf)

data SplitMode = Unsplit | Split
  deriving (Show)

suite :: Suite
suite =
  Suite
    { suiteName = "split",
      suiteDescription = "Independent root-component solving versus ordinary custom solving",
      suiteRuns = 3,
      suitePrograms =
        [ TestProgram "simple" simple,
          TestProgram "chain" chain,
          TestProgram "sort-1" $ radixsort 1 bytes,
          TestProgram "sort-2" $ radixsort 2 bytes
        ],
      suiteVariants =
        [ Variant "unsplit" (benchProgram Unsplit),
          Variant "split" (benchProgram Split)
        ],
      suiteHeader =
        printf
          "%-12s %-8s %-20s %-20s %-12s %-12s %-10s %s\n"
          "program" "mode" "median presolve [s]" "median custom [s]"
          "custom cost" "CBC cost" "execution" "planned component sizes"
    }

benchProgram :: SplitMode -> Int -> TestProgram -> IO ()
benchProgram mode runs (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ILPStages rawConstraints lowerWithAssignments lower = makeILPStages MemoryUsage' fusedILP
      originalProblem = Problem rawConstraints emptySubstitution
      originalVariables = allVars $ lowerWithAssignments originalProblem

      simplify constraints = case presolve constraints of
        Left _ -> error "split benchmark: presolve found an infeasible problem"
        Right problem -> problem

      solver problem = case mode of
        Unsplit -> (False, solveOptimal lower originalVariables problem)
        Split -> solveOptimalWithSplitWithReport lower originalVariables problem

  _ <- evaluate $ length rawConstraints
  presolveTimes <- mapM (const $ timePure simplify rawConstraints) [1 .. runs]

  let problem = simplify rawConstraints
      ilp@(ILP _ objective _ _ constants) = lower problem

  _ <- evaluate problem

  solverTimes <- mapM (const $ timePure (solverChecksum . solver) problem) [1 .. runs]
  cbcResult <- solve (MIP cbc) ilp

  referenceCost <- case cbcResult of
    Nothing -> error "split benchmark: CBC found the problem infeasible"
    Just reference -> pure $ evalExpr constants reference objective

  let (didSplit, result) = solver problem
  checkCost referenceCost result

  let execution
        | modeIsUnsplit mode = "unsplit"
        | didSplit = "split"
        | otherwise = "fallback"
      sizes = sort $ componentSizes lower originalVariables problem
      sizesLabel = if null sizes then "unsafe/empty" else show sizes

  case result of
    Left completionError -> error $ "split benchmark: " ++ show completionError
    Right optimal ->
      printf
        "%-12s %-8s %-20.6f %-20.6f %-12d %-12d %-10s %s\n"
        name (show mode) (median presolveTimes) (median solverTimes)
        (cost optimal) referenceCost execution sizesLabel


solverChecksum :: (Bool, Either CompletionError FeasibleSolution) -> Int
solverChecksum (_, Left completionError) =
  error $ "split benchmark: solver failed: " ++ show completionError
solverChecksum (_, Right feasible) = cost feasible + M.size (solution feasible)

checkCost :: Int -> Either CompletionError FeasibleSolution -> IO ()
checkCost _ (Left completionError) =
  error $ "split benchmark: solver failed: " ++ show completionError
checkCost referenceCost (Right feasible)
  | cost feasible == referenceCost = pure ()
  | otherwise = error $ "split benchmark: CBC cost mismatch: " ++ show (cost feasible) ++ " /= " ++ show referenceCost

modeIsUnsplit :: SplitMode -> Bool
modeIsUnsplit Unsplit = True
modeIsUnsplit Split = False
