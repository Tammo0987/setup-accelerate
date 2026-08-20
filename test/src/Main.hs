{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Array.Accelerate
import Data.Array.Accelerate.Backend ( inspectCompiler )
import Data.Array.Accelerate.LLVM.Native ( Native, run )
import qualified Prelude

foo :: Acc (Vector Int)
foo = map (* 2) (use (fromList (Z :. 10) [1 .. 10]))

main :: Prelude.IO ()
main = do
  Prelude.putStrLn $ inspectCompiler @Native foo
  Prelude.print $ run foo
