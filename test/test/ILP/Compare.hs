{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module ILP.Compare where

import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Objective, makeILP)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), solve)
import ILP.Programs (TestProgram (..))
import ILP.Trafo (evalExpr, toFusionILP)

compareCost :: Objective -> TestProgram -> IO (Maybe Int, Maybe Int)
compareCost obj (TestProgram _ f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      defaultILP@(ILP _ objFun _ _ consts) = makeILP obj fusedILP
      -- TODO: add presolver here later.
      presolvedILP = makeILP obj fusedILP
  defaultSolution <- solve (MIP cbc) defaultILP
  presolvedSolution <- solve (MIP cbc) presolvedILP
  let defaultCost = fmap (\sol -> evalExpr consts sol objFun) defaultSolution
      presolvedCost = fmap (\sol -> evalExpr consts sol objFun) presolvedSolution
  pure (defaultCost, presolvedCost)
