module Main where

import Control.Monad (unless)
import ILP.Compare (compareProgram, comparisonOk, renderComparison)
import ILP.Programs (TestProgram (..), programs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  oks <- sequence [check obj p | obj <- [minBound .. maxBound], p <- programs]
  unless (and oks) exitFailure
  where
    check obj p@(TestProgram name _) = do
      let label = show obj ++ " / " ++ name ++ ": "
      result <- compareProgram obj p
      case result of
        Nothing -> putStrLn (label ++ "no solution") >> pure False
        Just c -> do
          putStrLn $ label ++ renderComparison c ++ (if comparisonOk c then " ok" else " fail")
          pure (comparisonOk c)
