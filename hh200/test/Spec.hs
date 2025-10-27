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
