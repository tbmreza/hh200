{-# LANGUAGE OverloadedStrings #-}

module E2eLspSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Language.LSP.Test
import Control.Monad.IO.Class (liftIO)

spec :: TestTree
spec = testGroup "E2E LSP Tests"
    [ testCase "basic failing case" $ do
        runSession "hh200 --lsp-stdio" fullCaps "test" $ do
            _doc <- openDoc "Spec.hs" "hh200"
            liftIO $ assertFailure "Intentional basic case failure"
    ]
