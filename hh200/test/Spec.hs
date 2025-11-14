{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import           Data.Text (Text)
-- import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as Prim
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Hh200.Types as Hh
import qualified Hh200.Scanner as Hh

import Hh200.Cli

main :: IO ()
main = defaultMain $ testGroup "HUnit"
    -- [ test1 ]

  [ testLR
  , testBel
  -- , test1
  ]


testBel :: TestTree
testBel = testCase "BEL callsite" $ do
    mgr <- Prim.newManager tlsManagerSettings
    instant <- Prim.parseRequest "http://localhost"
    mtRespBody :: Prim.Response L8.ByteString <- Prim.httpLbs instant mgr

    ok <-  Hh.assertsAreOk HM.empty mtRespBody (rsFrom ["true", "true", "true"])
    neg <- Hh.assertsAreOk HM.empty mtRespBody (rsFrom ["true", "false"])

    case (ok, neg) of
        (True, False) -> pure ()
        all -> assertFailure (show all)

    where
    rsFrom :: [String] -> Maybe Hh.ResponseSpec
    rsFrom lines = Just $ Hh.ResponseSpec
      { Hh.statuses = []
      , Hh.output = []
      , Hh.captures = Hh.RhsDict HM.empty
      , Hh.asserts = lines
      }

testLR :: TestTree
testLR = testCase "lexer and parser" $ do
    let tokens = Hh.alexScanTokens "POST http://localhost:9999/echo.php\nAuthorization: Bearer \nTest: $.data.id\nUser-Agent: \"lite\""

    case Hh.parse tokens of
        Hh.ParseOk v -> do
            pure ()
        Hh.ParseFailed _ -> do
            assertFailure $ show tokens

test1 :: TestTree
test1 = testCase "hh200 analyze" $ do
    let normal = Args { call = False, source = Just "../examples/draft.hhs"
                      , version = False
                      , debugConfig = False
                      }

    let (Just path) = source normal

    Just analyzed <- runMaybeT $ do
        script <- Hh.analyze path
        pure script

    case analyzed == Hh.Script { Hh.config = Hh.defaultScriptConfig, Hh.callItems = [] } of
        _ -> assertFailure $ show analyzed


-- test2 :: TestTree
-- test2 = testCase "hh200 testOutsideWorld" $ do

-- test3 :: TestTree
-- test3 = testCase "hh200 present" $ do
