{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import Data.Maybe (isJust)

import qualified Hh200.Types as Hh
import qualified Hh200.Scanner as Hh

testEval :: TestTree
testEval = testCase "BEL" $ do
    -- `show (t :: Text)` does introduce double quote on both ends.
    let res :: Text = "http://localhost:9998/route.php"

    -- assertFailure $ "ok:" ++ res
    assertFailure $ ((Hh.show' res) ++ "tail")

testScanner :: TestTree
testScanner = testCase "lexer and parser" $ do
    -- let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\n"
    -- let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer "
    let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""

    case Hh.parse tokens of
        Hh.ParseOk v -> do
            assertFailure $ "ok:" ++ show v
        Hh.ParseFailed _ -> do
            assertFailure $ show tokens


main :: IO ()
main = defaultMain $ testGroup "File-based tests"
  [ testScanner
  -- , testEval
  ]
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NamedFieldPuns #-}
--
-- import Test.Tasty
-- import Test.Tasty.HUnit
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.IO.Class
-- import Control.Monad (unless)
-- import System.Directory (doesFileExist)
-- import System.FilePath ((</>))
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import Data.Maybe (isJust)
--
-- import qualified Hh200.Types as Hh
-- import qualified Hh200.Scanner as Hh
--
-- testEndToEndHttpM :: TestTree
-- testEndToEndHttpM = testCase "??: if RaceM recreates current concurrency well, retire HttpM" $ do
--   result <- runMaybeT $ do
--     -- script <- Hh.staticChecks1 (".." </> "examples" </> "download.hhs")
--     lead <-   Hh.testOutsideWorld script
--     liftIO $  Hh.present lead @?= "todo"
--   unless (isJust result) $ assertFailure ""
--
-- testEndToEnd :: TestTree
-- testEndToEnd = testCase "??" $ do
--   result <- runMaybeT $ do
--     -- script <- Hh.staticChecks1 (".." </> "examples" </> "download.hhs")
--     lead <-   Hh.testOutsideWorld script
--     liftIO $  Hh.present lead @?= "Lead {firstFailing = Nothing}"
--   unless (isJust result) $ assertFailure ""
--
-- readFileMaybe :: FilePath -> MaybeT IO T.Text
-- readFileMaybe path = do
--   exists <- liftIO $ doesFileExist path
--   if exists
--     then liftIO $ TIO.readFile path
--     else MaybeT $ return Nothing
--
-- testReadGoldenFile :: TestTree
-- testReadGoldenFile = testCase "golden.txt contains expected value" $ do
--   result <- runMaybeT $ do
--     content <- readFileMaybe ("test" </> "golden.txt")
--     liftIO $ content @?= "expected contents\n"
--   unless (isJust result) $
--     assertFailure "golden.txt does not exist"
--
--
-- testScanner :: TestTree
-- testScanner = testCase "lexer and parser" $ do
--     -- let tokens = Hh.alexScanTokens "GET http://localhost:9999/indx HTTP 404" in
--     -- let tokens = Hh.alexScanTokens "GET http://example.com/route HTTP 404"
--
--     let tokens = Hh.alexScanTokens "http://localhost:9999/abcdefnxyz"
--     -- let tokens = Hh.alexScanTokens "\"nama\" then \"name\""
--
--     case Hh.parse tokens of
--         -- Hh.ParseOk (Hh.Script { Hh.call_items = [Hh.CallItem {ci_response_spec = Just z, ..}] }) -> return ()
--         -- ??
--         Hh.ParseOk _ -> do
--             assertFailure $ "ok:" ++ show tokens
--         Hh.ParseFailed _ -> do
--             assertFailure $ show tokens
--
--
-- -- testScanner1 :: TestTree
-- -- testScanner1 = testCase "old" $ do
-- --
-- --     -- Hh.alexScanTokens "GET http://localhost:9999/indx" @?= []  -- ??: ok
-- --
-- --     -- let tokens = Hh.alexScanTokens "GET http://localhost:9999/indx  HTTP 404" in
-- --     let tokens = Hh.alexScanTokens "GET http://localhost:9999/indx" in
-- --         case Hh.parse tokens of
-- --             Hh.ParseOk (Hh.Script { Hh.call_items = [item] }) -> return ()
-- --             Hh.ParseFailed _ -> do
-- --                 assertFailure "todo"
--
-- main :: IO ()
-- main = defaultMain $ testGroup "File-based tests"
--   -- [ testReadGoldenFile
--   -- -- , testEndToEndHttpM
--   -- -- , testEndToEndWithThreads  -- ??
--   -- , testEndToEnd
--   -- ]
--   --
--   -- [ testScanner1
--   -- , testScanner
--   -- ]
--
--   [ testScanner
--   ]
