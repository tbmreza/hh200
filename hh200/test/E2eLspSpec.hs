{-# LANGUAGE OverloadedStrings #-}

module E2eLspSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Language.LSP.Test
import Control.Monad.IO.Class (liftIO)
-- import Control.Applicative (skipMany)

-- spec :: TestTree
-- spec = testGroup "E2E LSP Tests"
--     [ testCase "basic failing case" $ do
--         runSession "hh200 --lsp-stdio" fullCaps "test" $ do
--             doc <- openDoc "Spec.hs" "hh200"
--             skipMany anyNotification
--             symbols <- getDocumentSymbols doc
--             liftIO $ assertFailure ("Intentional basic case failure. Found " <> show (length symbols) <> " symbols.")
--     ]
