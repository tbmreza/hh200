module Main (main) where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Data.ByteString.Lazy (ByteString)
-- import System.IO
-- import qualified Data.ByteString.Lazy as BS
-- import Hh200.Cli
import Hh200.Vm (Vm, httpClientCall)
import Hh200.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTest, unitTests]

processData :: String -> ByteString
processData _ = "akk"

goldenTest :: TestTree
goldenTest = goldenVsString "Process Data Test" "test/golden/version.golden" $ do
    let input = "example"
    return $ processData input

t1 :: TestTree
t1 = testCase "POST with body" $ do
    echo <- preloadGet
    res <- httpClientCall preloadGet
    let expected = (True, echo)
    let actual = res
    assertEqual "" expected actual where
        preloadGet :: IO Vm
        preloadGet = do return ("POST", [], "http://localhost:9999/router.php", "body string...", 200, [X], False)

t2 :: TestTree
t2 = testCase "Get 200 from httpbin." $ do
    echo <- preloadGet
    res <- httpClientCall preloadGet
    let expected = (True, echo)
    let actual = res
    assertEqual "" expected actual where
        preloadGet :: IO Vm
        -- preloadGet = do return ("GET", [], "http://httpbin.org/anything", "", 200, [X], False)
        -- preloadGet = do return ("GET", [], "http://localhost:9999", "", 404, [X], False)
        preloadGet = do return ("GET", [], "http://httpbin.org/get", "", 200, [X], False)

t3 :: TestTree
t3 = testCase "httpClientCall callsite" $ do
    echo <- preloadGet
    res <- httpClientCall preloadGet
    let expected = (True, echo)
    let actual = res
    assertEqual "" expected actual where
        preloadGet :: IO Vm
        preloadGet = do return ("POST", [], "http://localhost:9999/router.php", "body string...", 200, [X], False)

unitTests :: TestTree
unitTests = testGroup "Unit tests" [t1, t2]
-- unitTests = testGroup "Unit tests" [t3]

-- preloadGet = do return ("GET", [], "http://localhost:9999", 200, [X], False)
-- preloadGet' :: IO Vm
-- preloadGet' = do return ("GET", [], "http://httpbin.org/anything", 400, [], False)
-- 10 @?= 10
