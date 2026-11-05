{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Adapts curl's HTTP conformance tests (vendored under @test\/curl\/data@)
-- into one tasty test case per test: @curl.testNNNN <keywords>@.
--
-- The replay server (ocapi @replay@) is started by @scripts/curl-tests.sh@,
-- which exports @OCAPI_REPLAY_DIR@ and @OCAPI_REPLAY_PORT@. When those are
-- unset this group is empty, so a plain @stack test@ stays self-contained.
--
-- For each test we: parse the pseudo-XML, render the @<reply>@ bytes into the
-- replay dir, translate @<command>@ into an hh200 script, run it against the
-- replay server, then check (a) the outcome (status assertion / HTTP error)
-- and (b) the request hh200 sent against @<verify><protocol>@ (normalized).
module CurlSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Exception (SomeException, fromException, try)
import           Control.Monad (forM)
import           Data.Char (isSpace, toLower)
import           Data.Either (partitionEithers)
import           Data.List (intercalate, isPrefixOf)
import           Data.Maybe (fromMaybe, listToMaybe)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import           System.Environment (lookupEnv)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Network.HTTP.Client as HC

import           Hh200.Scanner (analyze)
import           Hh200.Types
import           Hh200.Execution (AssertionFailure (..), runScriptM)

import           CurlTest.Types
import           CurlTest.Parse (parseCurlTest)
import           CurlTest.Preprocess (renderReply, substitute)
import           CurlTest.Translate (translate, tokenize)

spec :: TestTree
spec = testGroup "curl" (unsafePerformIO buildTests)
{-# NOINLINE spec #-}

-- | Path join (Linux-only; avoids pulling @filepath@ into the test-suite).
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- ---------------------------------------------------------------------------
-- Test discovery and fixture generation (runs once).
-- ---------------------------------------------------------------------------

data Loaded = Loaded Int CurlTest RequestPlan Expected

buildTests :: IO [TestTree]
buildTests = do
    mReplayDir <- lookupEnv "OCAPI_REPLAY_DIR"
    case mReplayDir of
        Nothing -> pure []
        Just replayDir -> do
            port <- fromMaybe "18081" <$> lookupEnv "OCAPI_REPLAY_PORT"
            dataDir <- fromMaybe "../test/curl/data" <$> lookupEnv "HH200_CURL_DATA_DIR"
            examplesDir <- fromMaybe "../test/curl/examples" <$> lookupEnv "HH200_CURL_EXAMPLES_DIR"
            exists <- doesDirectoryExist dataDir
            if not exists
                then pure []
                else do
                    let prt = read port :: Int
                    files <- filter isTestFile <$> listDirectory dataDir
                    createDirectoryIfMissing True replayDir
                    createDirectoryIfMissing True (replayDir </> "requests")
                    results <- forM files (loadOne dataDir replayDir prt)
                    let (skipped, loaded) = partitionEithers results
                        routes = [ (rpMethod rp ++ " " ++ rpTarget rp, n)
                                 | Loaded n _ rp _ <- loaded ]
                    writeManifest replayDir routes
                    let cases = [ mkTest prt replayDir examplesDir n rp expected ct
                                | Loaded n ct rp expected <- loaded ]
                    pure $ cases ++ [coverageTest (length files) cases skipped]

isTestFile :: FilePath -> Bool
isTestFile name = "test" `isPrefixOf` name && any (`elem` ['0' .. '9']) name

testNumber :: FilePath -> Maybe Int
testNumber name = readMaybe (dropWhile (not . (`elem` ['0' .. '9'])) name)

-- | Load one vendored test: parse it, decide skip/inclusion, translate it, and
-- write its replay fixture.
loadOne
    :: FilePath
    -> FilePath
    -> Int
    -> FilePath
    -> IO (Either (Int, String) Loaded)
loadOne dataDir replayDir port name = do
    content <- readFile (dataDir </> name)
    case testNumber name of
        Nothing -> pure (Left (0, "unparseable test number: " ++ name))
        Just n -> do
            let ctx = Context n "127.0.0.1" port
            case parseCurlTest n content of
                Left err -> pure (Left (n, "parse error: " ++ err))
                Right ct ->
                    case shouldSkip ct of
                        Just why -> pure (Left (n, why))
                        Nothing ->
                            case translate ctx ct of
                                Left err -> pure (Left (n, "translate error: " ++ err))
                                Right (rp, expected) -> do
                                    writeFixture replayDir ctx ct
                                    pure (Right (Loaded n ct rp expected))

-- | Auto-skip anything outside the supported subset.
shouldSkip :: CurlTest -> Maybe String
shouldSkip ct
    | ctServer ct /= "http" =
        Just ("non-HTTP server: " ++ ctServer ct)
    | not (null (replyParts (ctReply ct))) =
        Just "multi-request (numbered <dataN>)"
    | Just f <- firstUnsupportedFeature (ctFeatures ct) =
        Just ("unsupported feature: " ++ f)
    | Just fl <- firstUnsupportedFlag (tokenize (ctCommand ct)) =
        Just ("unsupported curl flag: " ++ fl)
    | otherwise = Nothing
  where
    firstUnsupportedFeature = listToMaybe . filter unsupportedFeature
    firstUnsupportedFlag = listToMaybe . filter (`elem` unsupportedFlags)
    unsupportedFeature f =
        f `elem` [ "SSL", "proxy", "cookies", "http/2", "http/3", "HTTP-auth"
                 , "digest", "NTLM", "GSS-API", "Kerberos", "SPNEGO", "libz", "brotli", "zstd" ]
    unsupportedFlags =
        [ "-L", "--location", "--resolve", "-c", "--cookie-jar", "-b", "--cookie"
        , "--tlsv1", "--tlsv1.0", "--tlsv1.1", "--tlsv1.2", "--tlsv1.3"
        , "--compressed", "-x", "--proxy", "--cacert", "--cert", "--key"
        , "-w", "--write-out", "-o", "--output", "-u", "--user", "--digest", "--ntlm" ]

-- | Write the replay bytes for @<data>@ into @<replay-dir>/<N>.part0@.
writeFixture :: FilePath -> Context -> CurlTest -> IO ()
writeFixture replayDir ctx ct =
    case replyData (ctReply ct) of
        Nothing -> pure ()
        Just sec -> C8.writeFile (replayDir </> show (ctNumber ct) ++ ".part0")
                                  (renderReply ctx sec)

-- | Write the routing manifest (@<METHOD> <target>@ -> test number).
writeManifest :: FilePath -> [(String, Int)] -> IO ()
writeManifest replayDir routes =
    writeFile (replayDir </> "manifest.json")
        ("{\"routes\":{" ++ intercalate "," [ show k ++ ":" ++ show v | (k, v) <- routes ] ++ "}}")

-- ---------------------------------------------------------------------------
-- The actual test case.
-- ---------------------------------------------------------------------------

data Outcome = Ok | HttpErr | AssertFail | Other String
    deriving (Show, Eq)

mkTest
    :: Int -> FilePath -> FilePath -> Int -> RequestPlan -> Expected -> CurlTest -> TestTree
mkTest port replayDir examplesDir n rp expected ct =
    testCase (pad n ++ " " ++ unwords (ctKeywords ct)) $ do
        script <- loadExample port examplesDir n
        outcome <- runAndClassify script
        assertOutcome expected outcome
        let mReqFile = replayDir </> "requests" </> show n ++ ".txt"
        has <- fileExists mReqFile
        if not has
            then pure ()  -- no recorded request (e.g. connection never made)
            else do
                actual <- C8.readFile mReqFile
                let ctx = Context n "127.0.0.1" port
                    (failures, findings) = checkProtocol ctx (vProtocol (ctVerify ct)) actual
                mapM_ (\f -> putStrLn ("[curl/" ++ show n ++ "] finding: " ++ f)) findings
                assertEqual ("protocol mismatch:\n" ++ unlines failures) [] failures

pad :: Int -> String
pad n = "test" ++ replicate (4 - length (show n)) '0' ++ show n

-- | Read the generated example for a test. The examples are the single source
-- of truth for what the adapter runs (and what hh200-docs shows); the replay
-- port is the only thing rewritten at runtime.
loadExample :: Int -> FilePath -> Int -> IO Script
loadExample port examplesDir n = do
    let path = examplesDir </> ("test" ++ show n ++ ".hhs")
    present <- doesFileExist path
    if not present
        then assertFailure ("missing generated example " ++ path
                            ++ "; run scripts/sync-curl-tests.sh")
        else do
            source <- readFile path
            let rewritten = replaceAll "127.0.0.1:18081"
                                       ("127.0.0.1:" ++ show port)
                                       source
            case analyze (Snippet (L8.pack rewritten)) of
                Left err -> assertFailure ("invalid generated example " ++ path ++ ": " ++ err)
                Right script -> pure script

replaceAll :: String -> String -> String -> String
replaceAll from to s =
    let (before, rest) = breakOnString from s
    in case rest of
         [] -> s
         _  -> before ++ to ++ replaceAll from to (drop (length from) rest)

runAndClassify :: Script -> IO Outcome
runAndClassify script = do
    r <- try (runScriptM script newEnv) :: IO (Either SomeException ())
    pure $ case r of
        Right () -> Ok
        Left e
            | Just (_ :: AssertionFailure) <- fromException e -> AssertFail
            | Just (_ :: HC.HttpException) <- fromException e -> HttpErr
            | otherwise -> Other (show e)

assertOutcome :: Expected -> Outcome -> IO ()
assertOutcome expected outcome =
    case (expected, outcome) of
        (ExpectOk, Ok)                 -> pure ()
        (ExpectHttpError, HttpErr)     -> pure ()
        (ExpectAssertFail, AssertFail) -> pure ()
        _ -> assertFailure ("expected " ++ show expected ++ ", got " ++ show outcome)

-- ---------------------------------------------------------------------------
-- Normalized request comparison against <verify><protocol>.
-- ---------------------------------------------------------------------------

-- | Headers whose values we require to match curl's <protocol>. Transport
-- headers curl auto-adds (User-Agent, Accept, Accept-Encoding, Expect, ...)
-- are ignored; extra hh200 headers in the whitelist are surfaced as findings.
whitelist :: [String]
whitelist = ["host", "content-type", "content-length"]

data HttpRequest = HttpRequest
    { hrMethod  :: String
    , hrTarget  :: String
    , hrHeaders :: [(String, String)]  -- ^ lowercased name, value
    , hrBody    :: C8.ByteString
    } deriving (Show)

checkProtocol :: Context -> Maybe Section -> C8.ByteString -> ([String], [String])
checkProtocol _ Nothing _ = ([], [])
checkProtocol ctx (Just sec) actual =
    let expected = parseExpected (substitute ctx (secBody sec))
        actual'  = parseActual actual
        failures =
            concat
                [ [ "method mismatch: expected " ++ show (hrMethod expected)
                        ++ ", got " ++ show (hrMethod actual')
                  | hrMethod expected /= hrMethod actual' ]
                , [ "target mismatch: expected " ++ show (hrTarget expected)
                        ++ ", got " ++ show (hrTarget actual')
                  | hrTarget expected /= hrTarget actual' ]
                , [ "body mismatch: expected " ++ show (hrBody expected)
                        ++ ", got " ++ show (hrBody actual')
                  | hrBody expected /= hrBody actual' ]
                , concatMap (checkHeader actual') (filter ((`elem` whitelist) . fst) (hrHeaders expected))
                ]
        findings = extraFindings expected actual'
    in (failures, findings)

checkHeader :: HttpRequest -> (String, String) -> [String]
checkHeader actual (name, value) =
    case lookup name (hrHeaders actual) of
        Just v | v == value -> []
        Just v -> ["header " ++ name ++ ": expected " ++ show value ++ ", got " ++ show v]
        Nothing -> ["missing header " ++ name ++ " (expected " ++ show value ++ ")"]

extraFindings :: HttpRequest -> HttpRequest -> [String]
extraFindings expected actual =
    let expectedNames = map fst (hrHeaders expected)
        extra = [ (n, v) | (n, v) <- hrHeaders actual
                         , n `elem` whitelist
                         , n `notElem` expectedNames ]
    in [ "hh200 sends extra header " ++ n ++ ": " ++ v | (n, v) <- extra ]

-- | Parse curl's <protocol> text (LF line endings).
parseExpected :: String -> HttpRequest
parseExpected s =
    let (headPart, bodyPart) = breakOn "\n\n" s
        ls = lines headPart
        (reqLine, hdrLines) = case ls of (r:hs) -> (r, hs); [] -> ("", [])
        (m, t) = case words reqLine of (m':t':_) -> (m', t'); _ -> ("", "")
    in HttpRequest m t (map parseHeader hdrLines) (C8.pack (drop 2 bodyPart))

-- | Parse the request ocapi recorded (CRLF head + raw body).
parseActual :: C8.ByteString -> HttpRequest
parseActual bs =
    let (headPart, bodyPart) = C8.breakSubstring "\r\n\r\n" bs
        headLines = map C8.unpack (splitOnBytes "\r\n" headPart)
        (reqLine, hdrLines) = case headLines of (r:hs) -> (r, hs); [] -> ("", [])
        (m, t) = case words reqLine of (m':t':_) -> (m', t'); _ -> ("", "")
    in HttpRequest m t (map parseHeader hdrLines) bodyPart

parseHeader :: String -> (String, String)
parseHeader line =
    let (name, value) = break (== ':') line
    in (map toLower (trim name), trim (drop 1 value))

-- ---------------------------------------------------------------------------
-- Reporting + small helpers.
-- ---------------------------------------------------------------------------

coverageTest :: Int -> [TestTree] -> [(Int, String)] -> TestTree
coverageTest nFiles cases skipped =
    testCase "coverage" $ do
        putStrLn ("[curl] vendored=" ++ show nFiles
            ++ " included=" ++ show (length cases)
            ++ " skipped=" ++ show (length skipped))
        mapM_ (\(n, r) -> putStrLn ("[curl] skip " ++ show n ++ ": " ++ r)) skipped

fileExists :: FilePath -> IO Bool
fileExists f = do
    r <- try (readFile f) :: IO (Either SomeException String)
    pure (either (const False) (const True) r)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle hay = go hay
  where
    go s
        | needle `isPrefixOf` s = ([], s)
        | otherwise =
            case s of
                [] -> ([], [])
                (c:cs) -> let (a, b) = go cs in (c : a, b)

splitOnBytes :: C8.ByteString -> C8.ByteString -> [C8.ByteString]
splitOnBytes delim bs =
    let (a, b) = C8.breakSubstring delim bs
    in if C8.null b then [a] else a : splitOnBytes delim b
