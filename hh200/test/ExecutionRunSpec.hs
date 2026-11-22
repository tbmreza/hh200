{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end behaviour of 'runScriptM' against a local server: the verdict a
-- script gets for status, header, JSON-body and @[Asserts]@ expectations.
--
-- These pin the observable outcome of the response checks in
-- "Hh200.Execution" so that refactors of that code path (e.g. removing the
-- @Debug.Trace@ calls around 'jsonSubset') cannot silently change verdicts.
module ExecutionRunSpec (spec) where

import           Control.Exception (SomeException, fromException, try)
import           Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.IO (hClose, hPutStr, hSetEncoding, openTempFile, utf8)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status (status418)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Web.Scotty as S

import           Hh200.Execution (AssertionFailure (..), CourierCtx (..), runScriptM, runScriptMWith)
import           Hh200.Inputs (FilePolicy (..), RunInputs (..), ScriptInputError (..), seedEnv)
import           Hh200.Curl (curlToScript)
import           Hh200.Types (CallItem (..), ResponseSpec (..), callItems, rpSquaresNothing, mtRhsDict)
import           Network.HTTP.Types.Status (status200)
import qualified CurlFrontendSpec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import           System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import           System.FilePath ((</>))
import           Hh200.Scanner (analyze)
import qualified Data.Text as Text
import           Hh200.Types (Script, Snippet (..), newEnv)

data Outcome = Ok | AssertFail | HttpErr | InputErr | Other String
    deriving (Show, Eq)

-- | A tiny fixed server the scripts run against.
fixtureApp :: S.ScottyM ()
fixtureApp = do
    S.get "/json" $ do
        S.setHeader "X-Probe" "yes"
        S.setHeader "Content-Type" "application/json"
        S.raw "{\"a\":1,\"b\":{\"c\":2},\"s\":\"x\"}"
    S.get "/teapot" $ do
        S.status status418
        S.text "short and stout"
    S.get "/empty" $ S.raw ""
    S.get "/tricky" $ S.json (object ["t" .= trickyText])
    -- Echoes what the request carried, so scripts can assert on it with the
    -- usual JSON-subset body check.
    S.post "/echo" $ do
        wreq <- S.request
        raw <- S.body
        let cts = [ v | (k, v) <- Wai.requestHeaders wreq, k == CI.mk "Content-Type" ]
            auth = lookup (CI.mk "Authorization") (Wai.requestHeaders wreq)
            parsed = decode raw :: Maybe Value
            subjectOk = case parsed of
                Just (Object o) -> KeyMap.lookup "subject" o == Just (String utf8Subject)
                _ -> False
            trickyOk = case parsed of
                Just (Object o) -> KeyMap.lookup "t" o == Just (String trickyText)
                _ -> False
        S.json $ object
            [ "contentTypeCount" .= length cts
            , "contentType" .= map TE.decodeUtf8 cts
            , "body" .= parsed
            , "subjectOk" .= subjectOk
            , "trickyOk" .= trickyOk
            , "authorization" .= fmap TE.decodeUtf8 auth
            , "rawBody" .= TE.decodeUtf8 (L8.toStrict raw)
            ]

-- | A subject with a Latin-1 and a non-Latin-1 character ("cafe" with an
-- acute e, then a check mark), which a Char8-encoded body would corrupt.
utf8Subject :: Text.Text
utf8Subject = "caf\233 \10003"

-- | Quotes, a backslash and non-ASCII: survives a capture and an interpolation
-- into a JSON body only if neither step escapes it the Haskell way.
trickyText :: Text.Text
trickyText = "say \"hi\" \\ caf\233 \10003"

-- | Run a script (with @PORT@ replaced by the fixture's port) and classify it.
runOn :: String -> IO Outcome
runOn src = do
    app <- S.scottyApp fixtureApp
    testWithApplication (pure app) $ \port -> do
        let rendered = replace "PORT" (show port) src
        parsed <- analyze (Snippet (L8.pack rendered))
        case parsed of
            Left err -> assertFailure ("fixture script does not parse: " ++ err ++ "\n" ++ rendered)
                        >> pure (Other "unparsed")
            Right script -> do
                classify script

classify :: Script -> IO Outcome
classify = classifyWith (runScriptM `flip` newEnv)

classifyWith :: (Script -> IO ()) -> Script -> IO Outcome
classifyWith run script = do
    r <- try (run script) :: IO (Either SomeException ())
    pure $ case r of
        Right () -> Ok
        Left e
            | Just (_ :: ScriptInputError) <- fromException e -> InputErr
            | Just (_ :: AssertionFailure) <- fromException e -> AssertFail
            | Just (_ :: HC.HttpException) <- fromException e -> HttpErr
            | otherwise -> Other (show e)

-- | Like 'runOn', but loads the script from a UTF-8 file (the path the CLI
-- takes), so non-ASCII text in the script survives.
runFileOn :: String -> IO Outcome
runFileOn src = do
    app <- S.scottyApp fixtureApp
    testWithApplication (pure app) $ \port -> do
        tmp <- getTemporaryDirectory
        (path, h) <- openTempFile tmp "exec-run.hhs"
        hSetEncoding h utf8
        hPutStr h (replace "PORT" (show port) src)
        hClose h
        parsed <- analyze path
        removeFile path
        case parsed of
            Left err -> assertFailure ("fixture script does not parse: " ++ err) >> pure (Other "unparsed")
            Right script -> classify script

replace :: String -> String -> String -> String
replace from to = go
  where
    go [] = []
    go s@(c:cs)
      | take (length from) s == from = to ++ go (drop (length from) s)
      | otherwise = c : go cs

expect :: Outcome -> String -> String -> TestTree
expect want name src = testCase name $ do
    got <- runOn src
    assertEqual ("outcome of:\n" ++ src) want got

spec :: TestTree
spec = testGroup "Execution (end-to-end verdicts)"
  [ testGroup "status"
      [ expect Ok "matching status passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n"
      , expect Ok "default expectation is 200"
          "GET http://127.0.0.1:PORT/json\n"
      , expect AssertFail "mismatched status fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 404\n"
      , expect Ok "non-200 status can be expected"
          "GET http://127.0.0.1:PORT/teapot\nHTTP 418\n"
      , expect AssertFail "unexpected non-200 fails"
          "GET http://127.0.0.1:PORT/teapot\nHTTP 200\n"
      ]
  , testGroup "json body (subset semantics)"
      [ expect Ok "expected body that is a strict subset passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\": 1}\n"
      , expect Ok "identical body passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\":1,\"b\":{\"c\":2},\"s\":\"x\"}\n"
      , expect Ok "identical body with reordered keys and whitespace passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{ \"s\": \"x\", \"b\": { \"c\": 2 }, \"a\": 1 }\n"
      , expect Ok "nested subset passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"b\": {\"c\": 2}}\n"
      , expect AssertFail "different value fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\": 2}\n"
      , expect AssertFail "key absent from response fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n{\"a\": 1, \"z\": 0}\n"
      , expect AssertFail "json expectation against a non-json body fails"
          "GET http://127.0.0.1:PORT/teapot\nHTTP 418\n{\"a\": 1}\n"
      , expect Ok "no body expectation skips the body check"
          "GET http://127.0.0.1:PORT/empty\nHTTP 200\n"
      ]
  , testGroup "headers"
      [ expect Ok "present response header passes"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\nX-Probe: \"yes\"\n"
      , expect AssertFail "wrong response header value fails"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\nX-Probe: \"no\"\n"
      ]
  , testGroup "request body (phase 0 regressions)"
      [ expect Ok "a script-set Content-Type is sent exactly once"
          "POST http://127.0.0.1:PORT/echo\nContent-Type: application/json\n{\"a\": 1}\nHTTP 200\n{\"contentTypeCount\": 1, \"contentType\": [\"application/json\"], \"body\": {\"a\": 1}}\n"
      , expect Ok "a JSON body without Content-Type defaults to application/json"
          "POST http://127.0.0.1:PORT/echo\n{\"a\": 1}\nHTTP 200\n{\"contentTypeCount\": 1, \"contentType\": [\"application/json\"]}\n"
      , expect Ok "a captured value is interpolated into the next request's body"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n[Captures]\nS: {{jsonpath \"$.s\"}}\n\nPOST http://127.0.0.1:PORT/echo\n{\"got\": \"{{S}}\"}\nHTTP 200\n{\"body\": {\"got\": \"x\"}}\n"
      , expect AssertFail "an interpolated body is checked, not sent literally"
          "GET http://127.0.0.1:PORT/json\nHTTP 200\n[Captures]\nS: {{jsonpath \"$.s\"}}\n\nPOST http://127.0.0.1:PORT/echo\n{\"got\": \"{{S}}\"}\nHTTP 200\n{\"body\": {\"got\": \"{{S}}\"}}\n"
      , expect Ok "a captured string with quotes, backslash and non-ASCII round-trips into a body"
          "GET http://127.0.0.1:PORT/tricky\nHTTP 200\n[Captures]\nT: {{jsonpath \"$.t\"}}\n\nPOST http://127.0.0.1:PORT/echo\n{\"t\": \"{{T}}\"}\nHTTP 200\n{\"trickyOk\": true}\n"
      , testCase "a non-Latin-1 body is sent as UTF-8" $ do
          got <- runFileOn $
              "POST http://127.0.0.1:PORT/echo\n{\"subject\": \"" ++ Text.unpack utf8Subject
              ++ "\"}\nHTTP 200\n{\"subjectOk\": true}\n"
          assertEqual "outcome" Ok got
      ]
  , phase1Tests
  , phase2Tests
  , testGroup "transport"
      [ expect HttpErr "connection refused is an HTTP error, not an assertion failure"
          "GET http://127.0.0.1:1/json\nHTTP 200\n"
      ]
  ]

-- ---------------------------------------------------------------------------
-- Phase 1: [Vars], file values, external variables, unbound names.
-- ---------------------------------------------------------------------------

-- | Bytes that are not valid UTF-8 (a PNG signature plus a NUL and 0xFF), so
-- only a byte-exact base64 path reproduces them.
photoBytes :: BS.ByteString
photoBytes = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0xFF, 0x10]

-- | Run with a temp directory holding @photo.png@, @note.txt@ and a @sub/@
-- directory. The callback gets the directory and builds the inputs.
withFixtureDir :: (FilePath -> IO a) -> IO a
withFixtureDir k = do
    tmp <- getTemporaryDirectory
    (marker, h) <- openTempFile tmp "hh200-phase1"
    hClose h
    removeFile marker
    let dir = marker ++ ".d"
    createDirectoryIfMissing True (dir </> "sub")
    BS.writeFile (dir </> "photo.png") photoBytes
    BS.writeFile (dir </> "note.txt") "hello from a file"
    r <- k dir
    removeDirectoryRecursive dir
    pure r

runWith :: RunInputs -> String -> IO Outcome
runWith inputs src = do
    app <- S.scottyApp fixtureApp
    testWithApplication (pure app) $ \port -> do
        let rendered = replace "PORT" (show port) src
        parsed <- analyze (Snippet (L8.pack rendered))
        case parsed of
            Left err -> assertFailure ("fixture script does not parse: " ++ err ++ "\n" ++ rendered)
                        >> pure (Other "unparsed")
            Right script ->
                let ctx = CourierCtx { courierName = "t", courierCapture = Nothing, courierFiles = riFiles inputs }
                in classifyWith (\s -> runScriptMWith ctx s (seedEnv inputs)) script

inputsAt :: FilePath -> RunInputs
inputsAt dir = RunInputs { riVars = HM.empty, riFiles = FilePolicy { fpBaseDir = dir, fpRoot = Nothing } }

expectIn :: Outcome -> String -> (FilePath -> RunInputs) -> String -> TestTree
expectIn want name mkInputs src = testCase name $ do
    got <- withFixtureDir (\dir -> runWith (mkInputs dir) src)
    assertEqual ("outcome of:\n" ++ src) want got

phase1Tests :: TestTree
phase1Tests = testGroup "phase 1: [Vars], files, external variables"
  [ expectIn Ok "base64-file, sends the file's bytes as base64" inputsAt $
      "POST http://127.0.0.1:PORT/echo\n[Vars]\nIMG: base64-file,photo.png\n"
      ++ "{\"attachments\": [{\"filename\": \"photo.png\", \"content\": \"{{IMG}}\"}]}\n"
      ++ "HTTP 200\n{\"body\": {\"attachments\": [{\"content\": \"" ++ photoBase64 ++ "\"}]}}\n"
  , expectIn Ok "file, sends the file's text" inputsAt $
      "POST http://127.0.0.1:PORT/echo\n[Vars]\nNOTE: file,note.txt\n{\"note\": \"{{NOTE}}\"}\n"
      ++ "HTTP 200\n{\"body\": {\"note\": \"hello from a file\"}}\n"
  , expectIn Ok "relative paths resolve against the policy's base directory, not the cwd"
      (\dir -> (inputsAt (dir </> "sub")))
      "POST http://127.0.0.1:PORT/echo\n[Vars]\nNOTE: file,../note.txt\n{\"note\": \"{{NOTE}}\"}\nHTTP 200\n{\"body\": {\"note\": \"hello from a file\"}}\n"
  , expectIn Ok "[Vars] values persist into later call items" inputsAt $
      "POST http://127.0.0.1:PORT/echo\n[Vars]\nX: hello\n{\"a\": 1}\nHTTP 200\n\n"
      ++ "POST http://127.0.0.1:PORT/echo\n{\"x\": \"{{X}}\"}\nHTTP 200\n{\"body\": {\"x\": \"hello\"}}\n"
  , expectIn Ok "an external variable reaches a header"
      (\dir -> (inputsAt dir) { riVars = HM.fromList [("API_KEY", String "re_test_123")] })
      "POST http://127.0.0.1:PORT/echo\nAuthorization: Bearer {{API_KEY}}\n{\"a\": 1}\nHTTP 200\n{\"authorization\": \"Bearer re_test_123\"}\n"
  , expectIn Ok "a [Vars] entry can use an external variable"
      (\dir -> (inputsAt dir) { riVars = HM.fromList [("NAME", String "world")] })
      "POST http://127.0.0.1:PORT/echo\n[Vars]\nGREETING: hello {{NAME}}\n{\"g\": \"{{GREETING}}\"}\nHTTP 200\n{\"body\": {\"g\": \"hello world\"}}\n"
    -- The refusals below point at port 1: had a request gone out, the outcome
    -- would be HttpErr, so InputErr shows nothing was sent.
  , expectIn InputErr "a path escaping the script root is refused before sending"
      (\dir -> FilePolicy (dir </> "sub") (Just (dir </> "sub")) `asFilesOf` inputsAt dir)
      "POST http://127.0.0.1:1/echo\n[Vars]\nIMG: base64-file,../photo.png\n{\"c\": \"{{IMG}}\"}\n"
  , expectIn InputErr "an absolute path outside the script root is refused"
      (\dir -> FilePolicy (dir </> "sub") (Just (dir </> "sub")) `asFilesOf` inputsAt dir)
      "POST http://127.0.0.1:1/echo\n[Vars]\nP: file,/etc/passwd\n{\"c\": \"{{P}}\"}\n"
  , expectIn Ok "a file under the script root is allowed"
      (\dir -> FilePolicy dir (Just dir) `asFilesOf` inputsAt dir)
      "POST http://127.0.0.1:PORT/echo\n[Vars]\nNOTE: file,note.txt\n{\"note\": \"{{NOTE}}\"}\nHTTP 200\n{\"body\": {\"note\": \"hello from a file\"}}\n"
  , expectIn InputErr "a missing file is refused before sending" inputsAt
      "POST http://127.0.0.1:1/echo\n[Vars]\nIMG: base64-file,nope.png\n{\"c\": \"{{IMG}}\"}\n"
  , expectIn InputErr "file, refuses non-UTF-8 bytes" inputsAt
      "POST http://127.0.0.1:1/echo\n[Vars]\nIMG: file,photo.png\n{\"c\": \"{{IMG}}\"}\n"
  , expectIn InputErr "an unbound {{NAME}} in a header fails before sending" inputsAt
      "POST http://127.0.0.1:1/echo\nAuthorization: Bearer {{RESEND_API_KEY}}\n{\"a\": 1}\n"
  , expectIn InputErr "an unbound {{NAME}} in the body fails before sending" inputsAt
      "POST http://127.0.0.1:1/echo\n{\"a\": \"{{MISSING}}\"}\n"
  , expectIn InputErr "an unbound {{NAME}} in the URL fails before sending" inputsAt
      "GET http://127.0.0.1:1/{{MISSING}}\n"
  , expectIn InputErr "a [Vars] entry can't use an unbound name" inputsAt
      "POST http://127.0.0.1:1/echo\n[Vars]\nA: x{{MISSING}}\n{\"a\": \"{{A}}\"}\n"
  , expectIn HttpErr "expressions aren't treated as unbound names" inputsAt
      "POST http://127.0.0.1:1/echo\n{\"n\": {{1 + 2}}}\n"
  ]
  where
    photoBase64 = L8.unpack (L8.fromStrict (B64.encode photoBytes))
    asFilesOf policy inputs = inputs { riFiles = policy }

-- ---------------------------------------------------------------------------
-- Phase 2: curl snippets, run without a shell.
-- ---------------------------------------------------------------------------

-- | Lower a curl snippet (with @PORT@ replaced), expect @expectBody@ as a
-- JSON subset of the echo, and run it under the given inputs.
runCurlWith :: (FilePath -> RunInputs) -> String -> String -> IO Outcome
runCurlWith mkInputs snippet expectBody = withFixtureDir $ \dir -> do
    app <- S.scottyApp fixtureApp
    testWithApplication (pure app) $ \port ->
        case curlToScript (replace "PORT" (show port) snippet) of
            Left d -> assertFailure ("snippet refused: " ++ show d) >> pure (Other "refused")
            Right (script, _) -> do
                let inputs = mkInputs dir
                    expecting ci = ci { ciResponseSpec = Just ResponseSpec
                        { rpStatuses = [status200], rpSquares = rpSquaresNothing
                        , rpResponseHeaders = mtRhsDict, rpBody = expectBody } }
                    script' = script { callItems = map expecting (callItems script) }
                    ctx = CourierCtx { courierName = "t", courierCapture = Nothing, courierFiles = riFiles inputs }
                classifyWith (\s -> runScriptMWith ctx s (seedEnv inputs)) script'

phase2Tests :: TestTree
phase2Tests = testGroup "phase 2: curl snippets"
  [ testCase "the Resend snippet sends one Content-Type, its Authorization, and the photo as base64" $ do
      got <- runCurlWith inputsAt (CurlFrontendSpec.resendSnippet "http://127.0.0.1:PORT/echo") $
          "{\"contentTypeCount\": 1, \"contentType\": [\"application/json\"]"
          ++ ", \"authorization\": \"Bearer re_YOUR_API_KEY\""
          ++ ", \"body\": {\"from\": \"sender@example.com\", \"attachments\": [{\"filename\": \"photo.png\", \"content\": \""
          ++ L8.unpack (L8.fromStrict (B64.encode photoBytes)) ++ "\"}]}}"
      assertEqual "outcome" Ok got
  , testCase "a $NAME credential comes from the external variables" $ do
      got <- runCurlWith
          (\dir -> (inputsAt dir) { riVars = HM.fromList [("RESEND_API_KEY", String "re_live_xyz")] })
          "curl -H \"Authorization: Bearer $RESEND_API_KEY\" --json '{\"a\": 1}' http://127.0.0.1:PORT/echo\n"
          "{\"authorization\": \"Bearer re_live_xyz\", \"contentType\": [\"application/json\"], \"body\": {\"a\": 1}}"
      assertEqual "outcome" Ok got
  , testCase "a missing $NAME stops the snippet before it sends anything" $ do
      got <- runCurlWith inputsAt "curl -H \"Authorization: Bearer $RESEND_API_KEY\" -d x http://127.0.0.1:1/echo\n" "{}"
      assertEqual "outcome" InputErr got
  , testCase "a form body interpolates raw text, not JSON" $ do
      got <- runCurlWith
          (\dir -> (inputsAt dir) { riVars = HM.fromList [("WHO", String "hello world")] })
          "curl -d 'a=1' -d \"b=$WHO\" http://127.0.0.1:PORT/echo\n"
          "{\"rawBody\": \"a=1&b=hello world\", \"contentType\": [\"application/x-www-form-urlencoded\"]}"
      assertEqual "outcome" Ok got
  , expectIn Ok "a {{NAME}} in a URL template renders in place (no extra '/')"
      (\dir -> (inputsAt dir) { riVars = HM.fromList [("P", String "json")] })
      "GET http://127.0.0.1:PORT/{{P}}\nHTTP 200\n{\"a\": 1}\n"
  ]
