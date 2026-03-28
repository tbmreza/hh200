{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Control.Concurrent.MVar (newMVar)

import qualified CliSpec
import qualified LspSpec
import qualified NetworkSpec
import qualified ContentTypeSpec
import qualified ScannerSpec

main :: IO ()
main = do
    lock <- newMVar ()
    defaultMain $ testGroup "Hh200 Tests"
      [ CliSpec.spec lock
      , LspSpec.spec
      , NetworkSpec.spec lock
      , ContentTypeSpec.spec
      , ScannerSpec.spec
      ]
