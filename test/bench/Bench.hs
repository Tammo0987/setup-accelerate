{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (evaluate)
import Criterion.Measurement (initializeTime, measure)
import Criterion.Measurement.Types (Measured (measTime), whnf, whnfIO)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.CustomSolver (FeasibleSolution (..), solveOptimal)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (ILPStages (..), Objective (..), makeILPStages)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Presolve (Problem (..), emptySubstitution, presolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), allVars, solve)
import Data.Either (fromRight)
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import ILP.Programs (TestProgram (..), bytes, radixsort, twoSorts)
import ILP.Trafo (evalExpr, toFusionILP)
import ILP.Validate (rows)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

objectives :: [Objective]
objectives = [MemoryUsage']

runs :: Int
runs = 2

customRuns :: Int
customRuns = 1

data PresolveMode
  = NoPresolve
  | AssignmentConstraints
  | ReplaceVariables
  deriving (Eq)

modeName :: PresolveMode -> String
modeName NoPresolve = "none"
modeName AssignmentConstraints = "assign"
modeName ReplaceVariables = "replace"

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

customSolverPrograms :: [TestProgram]
customSolverPrograms =
  [ TestProgram "sort-1" $ radixsort 1 bytes
  ]

data BenchmarkSuite
  = OriginalBenchmark
  | CustomSolverBenchmark
  | AllBenchmarks

parseBenchmarkSuite :: [String] -> IO BenchmarkSuite
parseBenchmarkSuite arguments =
  case arguments of
    [] ->
      pure OriginalBenchmark
    ["--custom"] ->
      pure CustomSolverBenchmark
    ["--all"] ->
      pure AllBenchmarks
    _ -> do
      hPutStrLn stderr "Usage: ilp-bench [--custom | --all]"
      exitFailure

main :: IO ()
main = do
  benchmarkSuite <- getArgs >>= parseBenchmarkSuite
  initializeTime

  case benchmarkSuite of
    OriginalBenchmark ->
      runOriginalBenchmark
    CustomSolverBenchmark ->
      runCustomSolverBenchmark
    AllBenchmarks -> do
      runOriginalBenchmark
      runCustomSolverBenchmark

runOriginalBenchmark :: IO ()
runOriginalBenchmark = do
  putStrLn ""
  printf
    "%-12s %-12s %-6s %-6s %-20s %-16s %-12s\n"
    "program"
    "mode"
    "rows"
    "cols"
    "median presolve [s]"
    "median solve [s]"
    "cost"
  mapM_ benchProgram benchPrograms

runCustomSolverBenchmark :: IO ()
runCustomSolverBenchmark = do
  putStrLn ""
  printf
    "%-12s %-20s %-20s %-12s %-12s %-12s\n"
    "program"
    "median presolve [s]"
    "median custom [s]"
    "optimal cost"
    "CBC cost"
    "assignments"
  mapM_ benchCustomSolver customSolverPrograms

benchProgram :: TestProgram -> IO ()
benchProgram (TestProgram name f) =
  sequence_
    [ run mode objective
    | objective <- objectives,
      mode <- [NoPresolve, AssignmentConstraints, ReplaceVariables]
    ]
  where
    fusedILP = toFusionILP @NativeOp defaultOptions f

    run mode objective = do
      let ILPStages rawConstraints lowerWithAssignments lowerWithReplacement = makeILPStages objective fusedILP
          usePresolve = mode /= NoPresolve

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

          lower = case mode of
            ReplaceVariables -> lowerWithReplacement
            _ -> lowerWithAssignments

          ilp@(ILP _ objective' constraint _ constants) = lower problem

      -- Warm up the solver and force the lowered ILP.
      _ <- solve (MIP cbc) ilp

      solveTimes <- mapM (const $ timeIO $ solve (MIP cbc) ilp) [1 .. runs]
      result <- solve (MIP cbc) ilp

      printf
        "%-12s %-12s %-6d %-6d %-20s %-16.4f %-12s\n"
        name
        (modeName mode)
        (length $ rows constraint)
        (S.size $ allVars ilp)
        ( case presolveTimes of
            [] -> "-"
            times -> printf "%.4f" (median times)
        )
        (median solveTimes)
        (maybe "infeasible" (show . (\solution -> evalExpr constants solution objective')) result)

benchCustomSolver :: TestProgram -> IO ()
benchCustomSolver (TestProgram name f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ILPStages rawConstraints lowerWithAssignments lowerWithReplacement = makeILPStages MemoryUsage' fusedILP
      originalProblem = Problem rawConstraints emptySubstitution
      originalVariables = allVars $ lowerWithAssignments originalProblem
      solver = solveOptimal lowerWithReplacement originalVariables

  -- Construct the raw constraint-list spine before timing presolve.
  _ <- evaluate $ length rawConstraints

  presolveTimes <-
    mapM (const $ timePure presolve rawConstraints) [1 .. customRuns]

  let presolvedProblem =
        fromRight
          (error "presolve: ILP is infeasible")
          (presolve rawConstraints)
      ILP _ objective _ _ constants = lowerWithReplacement presolvedProblem

  solverTimes <-
    mapM
      (const $ timePure (solverChecksum . solver) presolvedProblem)
      [1 .. customRuns]

  cbcResult <- solve (MIP cbc) (lowerWithReplacement presolvedProblem)

  case solver presolvedProblem of
    Left completionError ->
      printf
        "%-12s %-20.4f %-20.4f %-12s %-12s %-12s\n"
        name
        (median presolveTimes)
        (median solverTimes)
        "failed"
        "-"
        (show completionError)
    Right optimalSolution ->
      printf
        "%-12s %-20.4f %-20.4f %-12d %-12s %-12d\n"
        name
        (median presolveTimes)
        (median solverTimes)
        (cost optimalSolution)
        (maybe "infeasible" (show . (\solution -> evalExpr constants solution objective)) cbcResult)
        (M.size $ solution optimalSolution)

solverChecksum :: Either completionError FeasibleSolution -> Int
solverChecksum result =
  case result of
    Left _ -> -1
    Right feasibleSolution ->
      cost feasibleSolution + M.size (solution feasibleSolution)

timePure :: (a -> b) -> a -> IO Double
timePure function input = measTime . fst <$> measure (whnf function input) 1

timeIO :: IO a -> IO Double
timeIO action = measTime . fst <$> measure (whnfIO action) 1

median :: [Double] -> Double
median xs = sort xs !! (length xs `div` 2)
