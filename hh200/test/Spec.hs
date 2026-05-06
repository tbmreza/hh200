{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Control.Concurrent.MVar (newMVar)

import qualified CliSpec
import qualified LspSpec
import qualified DatabaseSpec
import qualified ScannerSpec
import qualified ExecutionSpec
import qualified MustacheSpec

main :: IO ()
main = do
    lock <- newMVar ()
    defaultMain $ testGroup "Hh200 Tests"
      -- [ ScannerSpec.spec ]

      [ CliSpec.spec lock
      , LspSpec.spec
      , DatabaseSpec.spec
      , ScannerSpec.spec
      , MustacheSpec.spec
      , ExecutionSpec.spec
      ]
