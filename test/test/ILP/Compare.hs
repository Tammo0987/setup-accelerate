{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module ILP.Compare where

import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo (toFusionILP)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Objective, makeILP)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), evalExpr, solve)
import ILP.Programs (TestProgram (..))

legacyCost :: Objective -> TestProgram -> IO (Maybe Int)
legacyCost obj (TestProgram _ f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      ilp@(ILP _ objFun _ _ consts) = makeILP obj fusedILP
  msol <- solve (MIP cbc) ilp
  pure (fmap (\sol -> evalExpr consts sol objFun) msol)
