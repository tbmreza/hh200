{-# LANGUAGE OverloadedStrings #-}

-- Tests for the static MCP grammar resource (`hh200://docs/hhs-grammar`).
--
-- Two concerns are covered:
--   1. Drift: every ```hhs fenced example in the embedded resource must still
--      parse through the real lexer + parser, so the doc cannot silently
--      diverge from the grammar.
--   2. Smoke: `resources/read` advertises the resource as `text/markdown` with
--      the embedded text under the expected URI.
module McpResourceSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (forM_)
import Data.Aeson (Object, Value, (.=), encode, object)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T

import Hh200.McpServer (grammarResourceContents, grammarResourceText, grammarResourceUri, readResource)
import Hh200.Types (Snippet (..))
import qualified Hh200.Scanner as Scanner

spec :: TestTree
spec = testGroup "McpResource"
  [ testCase "every hhs code block in the resource parses" driftTest
  , testCase "resources/read serves the grammar as text/markdown" smokeTest
  , testCase "resources/read routes the grammar URI" readResourceRoutes
  ]

-- Extract the body of every ```hhs fenced block, in order.
extractHhsBlocks :: Text -> [Text]
extractHhsBlocks = go . T.lines
  where
    go [] = []
    go (l:ls)
      | T.strip l == "```hhs" =
          let (body, rest) = span (not . isClose) ls
          in T.unlines body : go (drop 1 rest)
      | otherwise = go ls
    isClose l = "```" `T.isPrefixOf` T.strip l

driftTest :: IO ()
driftTest = do
    let blocks = extractHhsBlocks grammarResourceText
    assertBool "resource must contain at least 3 hhs examples" (length blocks >= 3)

    forM_ blocks $ \block -> do
        res <- Scanner.analyze (Snippet (L8.pack (T.unpack block)))
        case res of
            Right _ -> pure ()
            Left err -> assertFailure $
                "resource example failed to parse: " ++ err
                ++ "\n--- example ---\n" ++ T.unpack block

smokeTest :: IO ()
smokeTest = do
    assertBool "grammar text is non-empty" (not (T.null grammarResourceText))
    let encoded = BL.toStrict (encode grammarResourceContents)
    assertBool "advertises the grammar URI"
        (C8.pack (T.unpack grammarResourceUri) `C8.isInfixOf` encoded)
    assertBool "marks the resource as text/markdown"
        (C8.pack "text/markdown" `C8.isInfixOf` encoded)
    assertBool "embeds the grammar text"
        (C8.pack "# hh200 grammar" `C8.isInfixOf` encoded)

-- | Exercise the @resources\/read@ dispatch directly: a known URI returns the
-- grammar contents, an unknown URI returns empty contents.
readResourceRoutes :: IO ()
readResourceRoutes = do
    assertEqual "known URI returns the grammar"
        grammarResourceContents
        (readResource (reqWithUri grammarResourceUri))
    assertEqual "unknown URI returns empty contents"
        (object ["contents" .= ([] :: [Value])])
        (readResource (reqWithUri "hh200://unknown"))

reqWithUri :: Text -> Object
reqWithUri uri = KM.fromList [(fromText "params", object ["uri" .= uri])]
