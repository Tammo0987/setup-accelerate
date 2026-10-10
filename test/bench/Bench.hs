module Main where

import qualified Baseline
import qualified BinaryDefaults
import Criterion.Measurement (initializeTime)
import qualified Custom
import Framework (runSuite)
import qualified Lowering
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs

  suites <- case arguments of
    [] -> pure [Baseline.suite]
    ["--baseline"] -> pure [Baseline.suite]
    ["--custom"] -> pure [Custom.suite]
    ["--lower"] -> pure [Lowering.suite]
    ["--binary-defaults"] -> pure [BinaryDefaults.suite]
    ["--all"] -> pure [Baseline.suite, Custom.suite, Lowering.suite, BinaryDefaults.suite]
    _ -> do
      hPutStrLn stderr "Usage: ilp-bench [--baseline | --custom | --lower | --binary-defaults | --all]"
      exitFailure

  initializeTime
  mapM_ runSuite suites
