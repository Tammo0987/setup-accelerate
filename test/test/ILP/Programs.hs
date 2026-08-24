{-# LANGUAGE GADTs #-}

module ILP.Programs where

import Data.Array.Accelerate
import Data.Array.Accelerate.Trafo.Sharing (Afunction)
import qualified Prelude

data TestProgram where
  TestProgram :: (Afunction f) => Prelude.String -> f -> TestProgram

programs :: [TestProgram]
programs =
  [ TestProgram "simple" simple,
    TestProgram "chain" chain,
    TestProgram "twoFolds" twoFolds,
    TestProgram "loop" loop
  ]

vec :: Acc (Vector Int)
vec = use (fromList (Z :. 10) [1 .. 10])

mat :: Acc (Matrix Int)
mat = use (fromList (Z :. 5 :. 10) [1 .. 50])

simple :: Acc (Vector Int)
simple = map (* 2) vec

chain :: Acc (Vector Int)
chain = map (+ 1) (map (* 2) (map (subtract 1) vec))

twoFolds :: Acc (Vector Int)
twoFolds = zipWith (+) (fold (+) 0 mat) (fold (*) 1 mat)

loop :: Acc (Vector Int)
loop = awhile (\a -> unit (the (sum a) < 1000)) (map (+ 1)) vec
