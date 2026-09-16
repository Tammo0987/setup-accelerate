{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Measurement (initializeTime, measure)
import Criterion.Measurement.Types (Measured (measTime), whnfIO)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (Objective (..), makeILPWithPresolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), allVars, solve)
import Data.List (sort)
import qualified Data.Set as S
import ILP.Programs (TestProgram (..), bytes, radixsort, twoSorts)
import ILP.Trafo (toFusionILP)
import ILP.Validate (rows)
import Text.Printf (printf)

objective :: Objective
objective = MemoryUsage'

runs :: Int
runs = 5

benchPrograms :: [TestProgram]
benchPrograms =
  [ TestProgram "sort-1" $ radixsort 1 bytes,
    TestProgram "sort-4" $ radixsort 4 bytes,
    TestProgram "sort-8" $ radixsort 8 bytes,
    TestProgram "twoSorts" $ twoSorts 4,
    TestProgram "twoSorts" $ twoSorts 8
  ]


main :: IO ()
main = do
  initializeTime
  printf "%-12s %-9s %-6s %-6s %-16s\n" "program" "presolve" "rows" "cols" "median solve [s]"
  mapM_ benchProgram benchPrograms

benchProgram :: TestProgram -> IO ()
benchProgram (TestProgram name f) = mapM_ run [False, True]
  where
    fusedILP = toFusionILP @NativeOp defaultOptions f

    run presolve = do
        let ilp@(ILP _ _ constraint _ _) = makeILPWithPresolve presolve objective fusedILP
        _ <- solve (MIP cbc) ilp -- warm up
        times <- mapM (const $ time $ solve (MIP cbc) ilp) [1 .. runs]
        printf "%-12s %-9s %-6d %-6d %-16.3f\n" name (show presolve) (length $ rows constraint) (S.size $ allVars ilp) (median times)

    time act = measTime . fst <$> measure (whnfIO act) 1

median :: [Double] -> Double
median xs = sort xs !! (length xs `div` 2)
