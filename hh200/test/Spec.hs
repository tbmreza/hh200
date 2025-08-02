{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isJust)

import qualified Hh200.Types as Hh
import qualified Hh200.Scanner as Hh

testEndToEndHttpM :: TestTree
testEndToEndHttpM = testCase "??: if RaceM recreates current concurrency well, retire HttpM" $ do
  result <- runMaybeT $ do
    script <- Hh.staticChecks1 (".." </> "examples" </> "download.hhs")
    lead <-   Hh.testOutsideWorld script
    liftIO $  Hh.present lead @?= "todo"
  unless (isJust result) $ assertFailure ""

testEndToEnd :: TestTree
testEndToEnd = testCase "??" $ do
  result <- runMaybeT $ do
    script <- Hh.staticChecks1 (".." </> "examples" </> "download.hhs")
    lead <-   Hh.testOutsideWorld script
    liftIO $  Hh.present lead @?= "Lead {firstFailing = Nothing}"
  unless (isJust result) $ assertFailure ""

readFileMaybe :: FilePath -> MaybeT IO T.Text
readFileMaybe path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then liftIO $ TIO.readFile path
    else MaybeT $ return Nothing

testReadGoldenFile :: TestTree
testReadGoldenFile = testCase "golden.txt contains expected value" $ do
  result <- runMaybeT $ do
    content <- readFileMaybe ("test" </> "golden.txt")
    liftIO $ content @?= "expected contents\n"
  unless (isJust result) $
    assertFailure "golden.txt does not exist"

testScanner :: TestTree
testScanner = testCase "lexer and parser" $ do

    -- Hh.alexScanTokens "GET http://localhost:9999/indx" @?= []  -- ??: ok

    let tokens = Hh.alexScanTokens "GET http://localhost:9999/indx" in
        case Hh.parse tokens of
            Hh.ParseOk (Hh.Script { Hh.call_items = [item] }) -> return ()
            Hh.ParseFailed _ -> do
                assertFailure "todo"

main :: IO ()
main = defaultMain $ testGroup "File-based tests"
  -- [ testReadGoldenFile
  -- -- , testEndToEndHttpM
  -- -- , testEndToEndWithThreads  -- ??
  -- , testEndToEnd
  -- ]
  [ testScanner
  ]
