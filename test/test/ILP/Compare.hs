{-# LANGUAGE TypeApplications #-}

module ILP.Compare where

import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Encoding (Objective, makeILPWithPresolve)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (solve)
import ILP.Programs (TestProgram (..))
import ILP.Trafo (toFusionILP)
import ILP.Validate (Report (..), renderReport, reportOk, validate)

data Comparison = Comparison
  { original :: Report,
    presolved :: Report,
    presolvedOnOriginal :: Report
  }

compareProgram :: Objective -> TestProgram -> IO (Maybe Comparison)
compareProgram obj (TestProgram _ f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      defaultILP = makeILPWithPresolve False obj fusedILP
      presolvedILP = makeILPWithPresolve True obj fusedILP
  defaultSolution <- solve (MIP cbc) defaultILP
  presolvedSolution <- solve (MIP cbc) presolvedILP

  pure $ do
    o <- defaultSolution
    p <- presolvedSolution
    Just $ Comparison (validate defaultILP o) (validate presolvedILP p) (validate defaultILP p)

comparisonOk :: Comparison -> Bool
comparisonOk c =
  reportOk (original c) && reportOk (presolvedOnOriginal c) && reportCost (original c) == reportCost (presolvedOnOriginal c)

renderComparison :: Comparison -> String
renderComparison c =
  "original: "
    ++ renderReport (original c)
    ++ " | presolved: rows="
    ++ show (reportRows (presolved c))
    ++ " cost="
    ++ show (reportCost (presolvedOnOriginal c))
