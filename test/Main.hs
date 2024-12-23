module Main (main) where

import Control.Exception
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Data.ByteString.Lazy (ByteString)
-- import Hh200.Vm (Vm, httpClientCall, popInstr)
import Hh200.Vm
import Hh200.Types

main :: IO ()
main = defaultMain tests

-- Production-ready compiler is a compiler that refuses to produce binaries that don't do exactly
-- what the input source instructs.
-- The converse is true, provided it agrees with the input source author about what's possible.
--
-- Mechanically:
-- 1. `vmRun`ning a successful `toVm` output must be guaranteed. In other words, Vm module implements
--    internal error type thrown as an exception that's impossible to trigger by any user scripts.
-- 2. `toVm` failure communicates what the compiler sees problematic about its input.

p1 :: TestTree
p1 = testGroup "vmRun may throw" [

    testCase "vmRun non-throw" $ do
        let runnable = ("OPTIONS", [], "http://localhost:9999/ignore.php", "ignore...", 200, [X], False)
        result <- try (vmRun runnable) :: IO (Either TerribleException Vm)
        case result of
            Right _ -> assertBool "" True
            Left _ -> assertFailure ""

  , testCase "vmRun throws" $ do
        result <- try (vmRun ("OPTIONS", [], "http://localhost:9999/ignore.php", "ignore...", 200, [], False)) :: IO (Either TerribleException Vm)
        case result of
            Left _ -> assertBool "" True  -- ??: show the exception
            Right _ -> assertFailure ""

        return ()
    ]

p2 :: TestTree
p2 = testGroup "reliable toVm" [

    testCase "valid user script" $ do
        case True of
            True -> assertBool "" True
            False -> assertFailure ""

  , testCase "invalid user script" $ do
        return ()
    ]

tests :: TestTree
tests = testGroup "Tests" [goldenTest, unitTests]

processData :: String -> ByteString
processData _ = "akk"

goldenTest :: TestTree
goldenTest = goldenVsString "Process Data Test" "test/golden/version.golden" $ do
    let input = "example"
    return $ processData input

-- t1 :: TestTree
-- t1 = testCase "POST with body" $ do
--     echo <- preloadGet
--     res <- httpClientCall preloadGet
--     let expected = (True, echo)
--     let actual = res
--     assertEqual "" expected actual where
--         preloadGet :: IO Vm
--         preloadGet = do return ("POST", [], "http://localhost:9999/router.php", "body string...", 200, [X], False)
--
-- t2 :: TestTree
-- t2 = testCase "Get 200 from httpbin." $ do
--     echo <- preloadGet
--     res <- httpClientCall preloadGet
--     let expected = (True, echo)
--     let actual = res
--     assertEqual "" expected actual where
--         preloadGet :: IO Vm
--         -- preloadGet = do return ("GET", [], "http://httpbin.org/anything", "", 200, [X], False)
--         -- preloadGet = do return ("GET", [], "http://localhost:9999", "", 404, [X], False)
--         preloadGet = do return ("GET", [], "http://httpbin.org/get", "", 200, [X], False)
--
-- t3 :: TestTree
-- t3 = testCase "fallible vm method: popInstr success" $ do
--     let input = do return    ("OPTIONS", [], "http://localhost:9999/ignore.php", "ignore...", 200, [X], False) :: IO Vm
--     actual <- popInstr input
--     let expected = (Nothing, ("OPTIONS", [], "http://localhost:9999/ignore.php", "ignore...", 200, [], False))
--
--     assertEqual "" expected actual
--
-- t4 :: TestTree
-- t4 = testCase "fallible vm method: popInstr error" $ do
--     actual <- popInstr unfitVm
--     -- expect unchanged next and error message
--     inner <- unfitVm
--     let expected = (Just OutOfBounds, inner)
--
--     assertEqual "" expected actual where
--         unfitVm :: IO Vm
--         unfitVm = do return ("OPTIONS", [], "http://localhost:9999/ignore.php", "ignore...", 200, [], False)

unitTests :: TestTree
unitTests = testGroup "Unit tests" [p1, p2]

-- 10 @?= 10
