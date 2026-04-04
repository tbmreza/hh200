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

main :: IO ()
main = do
    lock <- newMVar ()
    defaultMain $ testGroup "Hh200 Tests"
      -- [ NetworkSpec.spec lock ]

      [ CliSpec.spec lock
      , LspSpec.spec
      -- , NetworkSpec.spec lock
      , ContentTypeSpec.spec
      , ScannerSpec.spec
      , ExecutionSpec.spec
      ]
