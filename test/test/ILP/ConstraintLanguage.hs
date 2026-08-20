module ILP.ConstraintLanguage where

import Control.Monad.State (State)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Graph (Var)
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (Constants, Constraint, Bounds)
import Data.Map (Map)

data Prop op = Linear (Constraint op)

data LowerEnv op = LowerEnv {
    lowerEnvBounds:: Map (Var op) (Int, Int),
    lowerEnvConstants:: Constants
}

type Lower op = State String (Constraint op, Bounds op)

lower :: LowerEnv op -> Prop op -> Lower op
lower = undefined
