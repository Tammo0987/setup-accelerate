{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module ILP.Trafo (TrafoFront, toFusionILP, evalExpr) where

import qualified Data.Map as M

import Data.Array.Accelerate.Trafo.Config (Config)
import Data.Array.Accelerate.Trafo.Lowering (LowerAcc, lowerAfun)
import qualified Data.Array.Accelerate.Trafo.LetSplit as LetSplit
import qualified Data.Array.Accelerate.Trafo.Sharing as Sharing
import Data.Array.Accelerate.Trafo.Sharing (Afunction)
import qualified Data.Array.Accelerate.Trafo.Operation.LiveVars as Operation
import qualified Data.Array.Accelerate.Trafo.Operation.Simplify as Operation
import Data.Array.Accelerate.Trafo.Operation.Bounds (OperationBounds, boundsOptimizeAfun)
import qualified Data.Array.Accelerate.Trafo.Partitioning.ILP.Graph as Graph
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Var (Var)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.LinearConstraint
  (Constants, Expression (..), Number (..))
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (Solution)

type TrafoFront op =
  ( LowerAcc op
  , Operation.SimplifyOperation op
  , OperationBounds op
  , Operation.SLVOperation op
  , Graph.MakesILP op
  )

toFusionILP :: forall op f. (Afunction f, TrafoFront op) => Config -> f -> Graph.FusionILP op
toFusionILP config
  = (\(ilp, _, _) -> ilp)
  . Graph.mkFullGraphF
  . (Operation.simplifyFun . Operation.stronglyLiveVariablesFun)
  . (Operation.simplifyFun . boundsOptimizeAfun)
  . (Operation.simplifyFun . Operation.simplifyFun . lowerAfun)
  . LetSplit.convertAfun
  . Sharing.convertAfunWith config

evalExpr :: Constants -> Solution -> Expression -> Int
evalExpr consts sol = go
  where
    go (Constant (Number f)) = f consts
    go (a :+ b)              = go a + go b
    go (Number f :* v)       = f consts * value v

    value :: Var -> Int
    value v = case M.lookup v sol of
      Just x  -> x
      Nothing -> error ("evalExpr: variable not in solution: " ++ show v)
