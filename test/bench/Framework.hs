module Framework (Variant (..), Suite (..), runSuite, median, timePure, timeIO) where

import Criterion.Measurement (measure)
import Criterion.Measurement.Types (Measured (measTime), whnf, whnfIO)
import Data.List (sort)
import ILP.Programs (TestProgram)

data Variant = Variant
  { variantName :: String,
    runVariant :: Int -> TestProgram -> IO ()
  }

data Suite = Suite
  { suiteName :: String,
    suiteDescription :: String,
    suiteRuns :: Int,
    suitePrograms :: [TestProgram],
    suiteVariants :: [Variant],
    suiteHeader :: IO ()
  }

runSuite :: Suite -> IO ()
runSuite suite = do
  putStrLn $ "Running suite: " ++ suiteName suite
  suiteHeader suite
  mapM_ runProgram $ suitePrograms suite
  where
    runProgram program =
      mapM_ (\variant -> runVariant variant (suiteRuns suite) program) (suiteVariants suite)

median :: [Double] -> Double
median [] = error "median: no measurements"
median xs = sort xs !! (length xs `div` 2)

timePure :: (a -> b) -> a -> IO Double
timePure function input =
  measTime . fst <$> measure (whnf function input) 1

timeIO :: IO a -> IO Double
timeIO action =
  measTime . fst <$> measure (whnfIO action) 1
