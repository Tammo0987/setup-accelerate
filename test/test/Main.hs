{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (unless)
import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (Objective, makeILP)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (solve)
import ILP.Programs (TestProgram (..), programs)
import ILP.Trafo (toFusionILP)
import ILP.Validate (renderReport, reportOk, validate)
import System.Exit (exitFailure)

main :: IO ()
main = do
  oks <- sequence [check obj p | obj <- [minBound .. maxBound], p <- programs]
  unless (and oks) exitFailure
  where
    check obj (TestProgram name f) = do
      let ilp = makeILP obj (toFusionILP @NativeOp defaultOptions f)
          label = show obj ++ " / " ++ name ++ ": "
      solution <- solve (MIP cbc) ilp
      case solution of
        Nothing -> do
          putStrLn $ label ++ "no solution"
          pure False
        Just sol -> do
          let report = validate ilp sol
          putStrLn $ label ++ renderReport report ++ (if reportOk report then " ok" else " fail")
          pure $ reportOk report
