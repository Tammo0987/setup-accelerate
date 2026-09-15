{-# LANGUAGE LambdaCase #-}

module ILP.Validate where

import Data.Array.Accelerate.Trafo.Partitioning.ILP.Var (Var (..))
import Data.Array.Accelerate.Trafo.Partitioning.ILP.LinearConstraint (Bounds (..), Constants, LinearConstraint (..))
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Solver (ILP (..), Solution, allVars)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import ILP.Trafo (evalExpr)

-- recursing through :&&:, dropping true constraint
rows :: LinearConstraint -> [LinearConstraint]
rows TrueConstraint = []
rows (a :&& b) = rows a <> rows b
rows row = [row]

boundList :: Bounds -> [Bounds]
boundList (a :<> b) = boundList a <> boundList b
boundList NoBounds = []
boundList bound = [bound]

boundVar :: Bounds -> Maybe Var
boundVar (Binary v) = Just v
boundVar (LowerUpper _ v _) = Just v
boundVar (Lower _ v) = Just v
boundVar (Upper v _) = Just v
boundVar _ = Nothing

rowViolated :: Constants -> Solution -> LinearConstraint -> Bool
rowViolated consts solution = \case
  a :<= b -> ev a > ev b
  a :>= b -> ev a < ev b
  a :== b -> ev a /= ev b
  _ -> False
  where
    ev = evalExpr consts solution

boundViolated :: Solution -> Bounds -> Bool
boundViolated solution = \case
  Binary v -> value v < 0 || value v > 1
  LowerUpper l v u -> value v < l || value v > u
  Lower l v -> value v < l
  Upper v u -> value v > u
  _ -> False
  where
    value v = case M.lookup v solution of
      Just x -> x
      Nothing -> error $ "bound violated: variable not in solution: " ++ show v

varKind :: Var -> String
varKind = \case
  Pi {} -> "Pi"
  Fused {} -> "Fused"
  IsManifest {} -> "IsManifest"
  ReadDir {} -> "ReadDir"
  WriteDir {} -> "WriteDir"
  InFoldSize {} -> "InFoldSize"
  OutFoldSize {} -> "OutFoldSize"
  InPlace {} -> "InPlace"
  PiMax {} -> "PiMax"
  MaxCluster {} -> "MaxCluster"
  Other name -> takeWhile (/= '_') name

data Report = Report
  { reportRows :: Int,
    reportColumns :: Int,
    reportColumnsByKind :: M.Map String Int,
    reportUnranged :: M.Map String Int,
    reportRowViolations :: [LinearConstraint],
    reportBoundViolations :: [Bounds],
    reportCost :: Int
  }

reportOk :: Report -> Bool
reportOk r = null (reportRowViolations r) && null (reportBoundViolations r)

validate :: ILP -> Solution -> Report
validate ilp@(ILP _ cost constraint bounds consts) solution =
  Report
    { reportRows = length allRows,
      reportColumns = S.size cols,
      reportColumnsByKind = countKinds cols,
      reportUnranged = countKinds $ S.difference cols ranged,
      reportRowViolations = filter (rowViolated consts solution) allRows,
      reportBoundViolations = filter (boundViolated solution) allBounds,
      reportCost = evalExpr consts solution cost
    }
  where
    allRows = rows constraint
    allBounds = boundList bounds
    cols = allVars ilp
    ranged = S.fromList $ mapMaybe boundVar allBounds

    countKinds = M.fromListWith (+) . map (\v -> (varKind v, 1 :: Int)) . S.toList

renderReport :: Report -> String
renderReport r =
  unwords
    [ "rows=" ++ show (reportRows r),
      "cols=" ++ show (reportColumns r),
      "cost=" ++ show (reportCost r),
      "rowViol=" ++ show (length (reportRowViolations r)),
      "bndViol=" ++ show (length (reportBoundViolations r)),
      "unranged=" ++ showKinds (reportUnranged r)
    ]
  where
    showKinds m
      | M.null m = "none"
      | otherwise = intercalate "," [k ++ ":" ++ show c | (k, c) <- M.toAscList m]
