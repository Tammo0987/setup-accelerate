{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module ILP.Compare where

import Data.Array.Accelerate.LLVM.Native.Operation (NativeOp)
import Data.Array.Accelerate.Trafo (toFusionILP)
import Data.Array.Accelerate.Trafo.Config (defaultOptions)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.MIP (MIP (..), cbc)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solve (Encoding (Legacy, Modern), Objective, makeILP, makeILPWith)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), evalExpr, solve)
import ILP.Programs (TestProgram (..))

compareCost :: Objective -> TestProgram -> IO (Maybe Int, Maybe Int)
compareCost obj (TestProgram _ f) = do
  let fusedILP = toFusionILP @NativeOp defaultOptions f
      legacyILP@(ILP _ objFun _ _ consts) = makeILPWith Legacy obj fusedILP
      newILP = makeILPWith Modern obj fusedILP
  legacySolution <- solve (MIP cbc) legacyILP
  newSolution <- solve (MIP cbc) newILP
  let legacyCost = fmap (\sol -> evalExpr consts sol objFun) legacySolution
      newCost = fmap (\sol -> evalExpr consts sol objFun) newSolution
  pure (legacyCost, newCost)
