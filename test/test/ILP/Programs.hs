{-# LANGUAGE GADTs #-}

module ILP.Programs where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits (shiftR, xor, (.&.))
import Data.Array.Accelerate.Trafo.Sharing (Afunction)
import qualified Prelude

data TestProgram where
  TestProgram :: (Afunction f) => Prelude.String -> f -> TestProgram

programs :: [TestProgram]
programs =
  [ TestProgram "simple" simple,
    TestProgram "chain" chain,
    TestProgram "twoFolds" twoFolds,
    TestProgram "loop" loop,
    TestProgram "twoSorts" (twoSorts 1)
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

bytes :: Acc (Vector Word8)
bytes = use (fromList (Z :. 10) [7, 3, 9, 1, 8, 2, 6, 0, 5, 4])

twoSorts :: Int -> Acc (Vector Word8)
twoSorts bits = zipWith (+) (radixsort bits bytes) (radixsort bits (map (* 3) bytes))

radixsort :: Int -> Acc (Vector Word8) -> Acc (Vector Word8)
radixsort bits = Prelude.foldr1 (>->) (Prelude.map radixPass [0 .. bits Prelude.- 1])
  where
    radixPass :: Int -> Acc (Vector Word8) -> Acc (Vector Word8)
    radixPass k v =
      let flags = map (\e -> fromIntegral ((e `shiftR` constant k) .&. 1)) v :: Acc (Vector Int)
          idown = prescanl (+) 0 (map (xor 1) flags)
          iup = map (size v - 1 -) (prescanr (+) 0 flags)
          index = zipWith (\f (T2 d u) -> f == 0 ? (d, u)) flags (zip idown iup)
       in permute const v (\ix -> Just_ (I1 (index ! ix))) v
