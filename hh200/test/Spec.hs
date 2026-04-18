{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Control.Concurrent.MVar (newMVar)

import qualified CliSpec
import qualified LspSpec
import qualified NetworkSpec
import qualified ContentTypeSpec
import qualified ScannerSpec
import qualified ExecutionSpec
import qualified E2eLspSpec

main :: IO ()
main = do
    lock <- newMVar ()
    defaultMain $ testGroup "Hh200 Tests"
      [ ScannerSpec.spec ]

      -- -- [ CliSpec.spec lock
      -- [ LspSpec.spec
      -- -- , E2eLspSpec.spec  -- ??:
      -- -- , NetworkSpec.spec lock
      -- , ContentTypeSpec.spec
      -- , ScannerSpec.spec
      -- , ExecutionSpec.spec
      -- ]
