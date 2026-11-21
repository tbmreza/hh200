{-# LANGUAGE OverloadedStrings #-}

-- | Every @.hhs@ script the repository ships as documentation must parse:
-- the files under @examples/@ and the fenced @hhs@ blocks in
-- @src/Hh200/hhs-grammar.md@. Guards against grammar changes (or doc edits)
-- that leave published examples broken.
module CorpusSpec (spec) where

import           Control.Monad (filterM, forM)
import           Data.List (isPrefixOf, isSuffixOf, sort)
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath ((</>))
import           System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, utf8, withFile)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Hh200.Scanner (analyze)
import           Hh200.Types (Snippet (..))

-- | Relative to the package directory, which is where @stack test@ runs.
examplesRoot :: FilePath
examplesRoot = ".." </> "examples"

grammarDoc :: FilePath
grammarDoc = "src" </> "Hh200" </> "hhs-grammar.md"

spec :: TestTree
spec = testGroup "Corpus"
  [ testGroup "examples/*.hhs parse" (unsafePerformIO exampleTests)
  , testGroup "hhs-grammar.md code blocks parse" (unsafePerformIO grammarDocTests)
  ]
{-# NOINLINE spec #-}

exampleTests :: IO [TestTree]
exampleTests = do
    files <- hhsFilesUnder examplesRoot
    pure $ if null files
        then [testCase "examples directory is present" (assertFailure ("no .hhs files under " ++ examplesRoot))]
        else [ testCase f (readUtf8 f >>= assertParses f) | f <- files ]

grammarDocTests :: IO [TestTree]
grammarDocTests = do
    doc <- readUtf8 grammarDoc
    let blocks = hhsBlocks (lines doc)
    pure $ if null blocks
        then [testCase "grammar doc has hhs examples" (assertFailure "no ```hhs blocks found")]
        else [ testCase ("block " ++ show i) (assertParses ("block " ++ show i) b)
             | (i, b) <- zip [1 :: Int ..] blocks ]

assertParses :: String -> String -> Assertion
assertParses label src = do
    r <- analyze (Snippet (L8.pack src))
    case r of
        Right _  -> pure ()
        Left err -> assertFailure (label ++ " does not parse: " ++ err ++ "\n---\n" ++ src)

-- | Fenced blocks opened with exactly @```hhs@.
hhsBlocks :: [String] -> [String]
hhsBlocks [] = []
hhsBlocks (l:ls)
    | l == "```hhs" =
        let (body, rest) = break ("```" `isPrefixOf`) ls
        in unlines body : hhsBlocks (drop 1 rest)
    | otherwise = hhsBlocks ls

hhsFilesUnder :: FilePath -> IO [FilePath]
hhsFilesUnder dir = do
    present <- doesDirectoryExist dir
    if not present then pure [] else do
        entries <- map (dir </>) . sort <$> listDirectory dir
        dirs <- filterM doesDirectoryExist entries
        nested <- forM dirs hhsFilesUnder
        pure (filter (".hhs" `isSuffixOf`) entries ++ concat nested)

-- | Read a file as UTF-8 whatever the locale (the grammar doc is not ASCII).
readUtf8 :: FilePath -> IO String
readUtf8 fp = withFile fp ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    length s `seq` pure s
