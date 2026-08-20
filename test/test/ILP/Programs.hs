{-# LANGUAGE GADTs #-}

module ILP.Programs where

import Data.Array.Accelerate
import Data.Array.Accelerate.Trafo.Sharing (Afunction)
import qualified Prelude

data TestProgram where
  TestProgram :: (Afunction f) => Prelude.String -> f -> TestProgram

programs :: [TestProgram]
programs = [TestProgram "simple" simple]

simple :: Acc (Vector Int)
simple = map (* 2) (use (fromList (Z :. 10) [1 .. 10]))
