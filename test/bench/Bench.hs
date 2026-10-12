module Main where

import qualified Baseline
import qualified Defaults
import Criterion.Measurement (initializeTime)
import qualified Custom
import Framework (runSuite)
import qualified Lowering
import qualified Splitting
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
    ["--defaults"] -> pure [Defaults.suite]
    ["--split"] -> pure [Splitting.suite]
    ["--all"] -> pure [Baseline.suite, Custom.suite, Lowering.suite, Defaults.suite, Splitting.suite]
    _ -> do
      hPutStrLn stderr "Usage: ilp-bench [--baseline | --custom | --lower | --defaults | --split | --all]"
      exitFailure

  initializeTime
  mapM_ runSuite suites
