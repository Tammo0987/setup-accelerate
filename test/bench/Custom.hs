{-# LANGUAGE TypeApplications #-}

module Custom (suite) where

import Control.Exception.Base (evaluate)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.CustomSolver (CompletionError, FeasibleSolution (..), solveOptimal)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (Problem (..), emptySubstitution, presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (ILP), allVars, solve)
import qualified Data.Map as M
import Framework (Suite (..), Variant (..), median, timePure)
import ILP.Programs (TestProgram (..), bytes, radixsort)
import ILP.Trafo (evalExpr, toFusionILP)
import Text.Printf (printf)

suite :: Suite
suite =
  Suite
    { suiteName = "custom",
      suiteDescription = "Custom solver benchmark",
      suiteRuns = 1,
      suitePrograms =
        [ TestProgram "sort-1" $ radixsort 1 bytes
        ],
      suiteVariants =
        [ Variant "custom" benchProgram
        ],
      suiteHeader =
        printf
          "%-12s %-20s %-20s %-12s %-12s\n"
          "program"
          "median presolve [s]"
          "median custom [s]"
          "custom cost"
          "CBC cost"
    }

benchProgram :: Int -> TestProgram -> IO ()
benchProgram runs (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f

      ILPStages rawConstraints lowerWithAssignments lower = makeILPStages MemoryUsage' fusedILP
      originalProblem = Problem rawConstraints emptySubstitution

      originalVariables = allVars $ lowerWithAssignments originalProblem

      solver = solveOptimal lower originalVariables

      simplify constraints = case presolve constraints of
        Left _ -> error "custom benchmark: presolve found an infeasible problem"
        Right problem -> problem

  _ <- evaluate $ length rawConstraints

  presolveTimes <- mapM (const $ timePure simplify rawConstraints) [1 .. runs]

  let problem = simplify rawConstraints
      ilp@(ILP _ objective _ _ constants) = lower problem

  _ <- evaluate problem

  solverTimes <- mapM (const $ timePure (solverChecksum . solver) problem) [1 .. runs]

  cbcResult <- solve (MIP cbc) ilp

  case (solver problem, cbcResult) of
    (Left completionError, _) ->
      error $ "custom benchmark: solver failed with error: " ++ show completionError
    (_, Nothing) ->
      error "custom benchmark: CBC solver found the problem infeasible"
    (Right optimal, Just reference) -> do
      let referenceCost = evalExpr constants reference objective

      if cost optimal /= referenceCost
        then error $ "cbc oracle cost mismatch: " ++ show (cost optimal) ++ " /= " ++ show referenceCost
        else
          printf
            "%-12s %-20.6f %-20.6f %-12d %-12d\n"
            name
            (median presolveTimes)
            (median solverTimes)
            (cost optimal)
            referenceCost

solverChecksum :: Either CompletionError FeasibleSolution -> Int
solverChecksum (Left _) = error "custom benchmark: solver failed"
solverChecksum (Right feasible) = cost feasible + M.size (solution feasible)
