module BlindLsp where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Silently (capture_)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8

import Hh200.Types as Hh
import Hh200.Scanner as Hh
import Hh200.Execution as Hh

import Hh200.Cli


spec :: TestTree
spec = testGroup "LSP states"
  [ testLSP_state
  ]

testLSP_state :: TestTree
testLSP_state = testCase "lexer state" $ do
    let input = "GET /"
        action :: Hh.Alex (Hh.Token, Int)
        action = do
            -- Set state
            Hh.alexSetUserState (Hh.AlexUserState { Hh.usCount = 42 })
            -- Read token (consumes "GET")
            t <- Hh.alexMonadScan
            -- Read state
            us <- Hh.alexGetUserState
            return (t, Hh.usCount us)

    case Hh.runAlex input action of
        Right (tok, count) -> do
            assertEqual "State check" 42 count
            case tok of
               Hh.METHOD _ "GET" -> pure ()
               _ -> assertFailure $ "Unexpected token: " ++ show tok
        Left err -> assertFailure $ "Lexer error: " ++ err
