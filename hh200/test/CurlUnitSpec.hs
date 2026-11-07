-- | Direct unit tests for the pure curl test-suite adapter modules
-- (@CurlTest.Parse@, @CurlTest.Preprocess@, @CurlTest.Translate@).
--
-- These exercise the parser/translator/preprocessor logic that the
-- integration harness (@CurlSpec@) reaches only through the gated replay
-- path, so they add coverage under a plain @stack test@.
module CurlUnitSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as C8

import           CurlTest.Types
import           CurlTest.Parse
import           CurlTest.Preprocess (applyCrlf, replyStatus, substitute)
import           CurlTest.Translate (detectBody, detectMethod, tokenize, translate, urlToTarget)

spec :: TestTree
spec = testGroup "CurlTest (pure)"
    [ parseTests
    , preprocessTests
    , translateTests
    ]

ctx :: Context
ctx = Context 42 "127.0.0.1" 18081

-- | A @CurlTest@ with all fields neutral, for the translate/detect tests.
emptyCurlTest :: CurlTest
emptyCurlTest = CurlTest
    { ctNumber   = 0
    , ctKeywords = []
    , ctName     = ""
    , ctServer   = "http"
    , ctFeatures = []
    , ctCommand  = ""
    , ctStdin    = Nothing
    , ctReply    = emptyReply
    , ctVerify   = emptyVerify
    }

-- ---------------------------------------------------------------------------
-- Parse
-- ---------------------------------------------------------------------------

parseTests :: TestTree
parseTests = testGroup "Parse"
    [ testCase "parseCurlTest extracts the supported subset" $ do
        case parseCurlTest 1 minimalTest of
            Left err -> assertFailure ("parse error: " ++ err)
            Right ct -> do
                assertEqual "number" 1 (ctNumber ct)
                assertEqual "keywords" ["HTTP", "HTTP GET"] (ctKeywords ct)
                assertEqual "server" "http" (ctServer ct)
                assertEqual "name" "HTTP GET\n" (ctName ct)
                assertEqual "command" "http://%HOSTIP:%HTTPPORT/%TESTNUMBER\n" (ctCommand ct)
                assertEqual "reply status" (Just 200)
                    (replyData (ctReply ct) >>= replyStatus)
                assertBool "protocol present"
                    (case vProtocol (ctVerify ct) of Just _ -> True; Nothing -> False)

    , testCase "parseCurlTest fails without a <testcase>" $
        case parseCurlTest 1 "" of
            Left _  -> pure ()
            Right _ -> assertFailure "expected a parse failure"

    , testCase "isOpenTag/isCloseTag match exact tag names" $ do
        assertBool "open exact" (isOpenTag "data" "<data crlf=\"headers\">")
        assertBool "open rejects numbered suffix" (not (isOpenTag "data" "<data2>"))
        assertBool "close exact" (isCloseTag "data" "</data>")
        assertBool "close rejects numbered suffix" (not (isCloseTag "data" "</data2>"))

    , testCase "attrsOf parses key=value pairs" $
        assertEqual "attrs" [("crlf", "headers"), ("mode", "text")]
            (attrsOf "crlf=\"headers\" mode=\"text\"")

    , testCase "sectionBody returns the inner lines" $
        assertEqual "keywords body" (Just ["HTTP", "HTTP GET"])
            (sectionBody "keywords" ["<keywords>", "HTTP", "HTTP GET", "</keywords>"])

    , testCase "numberedSections collects <dataN> sections" $ do
        let secs = numberedSections "data"
                ["<data1>", "a", "</data1>", "<data2>", "b", "</data2>"]
        assertEqual "two sections" 2 (length secs)
        assertEqual "names" ["data1", "data2"] (map secName secs)
    ]

-- | A minimal but representative curl pseudo-XML test file.
minimalTest :: String
minimalTest = unlines
    [ "<testcase>"
    , "<info>"
    , "<keywords>"
    , "HTTP"
    , "HTTP GET"
    , "</keywords>"
    , "</info>"
    , "<reply>"
    , "<data>"
    , "HTTP/1.1 200 OK"
    , ""
    , "-foo-"
    , "</data>"
    , "</reply>"
    , "<client>"
    , "<server>"
    , "http"
    , "</server>"
    , "<name>"
    , "HTTP GET"
    , "</name>"
    , "<command>"
    , "http://%HOSTIP:%HTTPPORT/%TESTNUMBER"
    , "</command>"
    , "</client>"
    , "<verify>"
    , "<protocol>"
    , "GET /%TESTNUMBER HTTP/1.1"
    , "</protocol>"
    , "</verify>"
    , "</testcase>"
    ]

-- ---------------------------------------------------------------------------
-- Preprocess
-- ---------------------------------------------------------------------------

preprocessTests :: TestTree
preprocessTests = testGroup "Preprocess"
    [ testCase "substitute expands macros" $ do
        assertEqual "host/port/number"
            "http://127.0.0.1:18081/42"
            (substitute ctx "http://%HOSTIP:%HTTPPORT/%TESTNUMBER")
        assertEqual "special characters"
            "a b\tc\rd&e><"
            (substitute ctx "a%SPb%TABc%CRd%AMPe%GT%LT")

    , testCase "applyCrlf honors the crlf attribute" $ do
        assertEqual "yes" "a\r\nb" (applyCrlf (Just "yes") "a\nb")
        assertEqual "unset" "a\nb" (applyCrlf Nothing "a\nb")

    , testCase "replyStatus reads the status line" $
        assertEqual "200" (Just 200)
            (replyStatus (Section "data" [] "HTTP/1.1 200 OK\nbody"))
    ]

-- ---------------------------------------------------------------------------
-- Translate
-- ---------------------------------------------------------------------------

translateTests :: TestTree
translateTests = testGroup "Translate"
    [ testCase "tokenize keeps quoted spans intact" $
        assertEqual "tokens"
            ["curl", "-X", "PUT", "http://example.com/a b"]
            (tokenize "curl -X PUT \"http://example.com/a b\"")

    , testCase "urlToTarget reduces a URL to its request-target" $ do
        assertEqual "query-only path" "/?mooo/42" (urlToTarget "http://127.0.0.1:18081/?mooo/42")
        assertEqual "root" "/" (urlToTarget "http://example.com")
        assertEqual "path" "/a/b" (urlToTarget "http://example.com/a/b")

    , testCase "detectMethod derives the method" $ do
        assertEqual "default" "GET" (detectMethod "curl http://x/")
        assertEqual "-X override" "PUT" (detectMethod "curl -X PUT http://x/")
        assertEqual "-d implies POST" "POST" (detectMethod "curl -d body http://x/")
        assertEqual "-T implies PUT" "PUT" (detectMethod "curl -T file http://x/")

    , testCase "detectBody prefers <stdin>, then a -d flag" $ do
        assertEqual "stdin" (C8.pack "hello") (detectBody emptyCurlTest { ctStdin = Just "hello" })
        assertEqual "-d" (C8.pack "body") (detectBody emptyCurlTest { ctCommand = "curl -d body http://x/" })
        assertEqual "none" (C8.pack "") (detectBody emptyCurlTest)

    , testCase "translate derives plan + expected outcome" $ do
        let ct = emptyCurlTest
                { ctCommand = "http://%HOSTIP:%HTTPPORT/%TESTNUMBER"
                , ctReply   = Reply (Just (Section "data" [] "HTTP/1.1 200 OK\nbody")) []
                }
        case translate ctx ct of
            Left err -> assertFailure ("translate error: " ++ err)
            Right (rp, expected) -> do
                assertEqual "method" "GET" (rpMethod rp)
                assertEqual "target" "/42" (rpTarget rp)
                assertEqual "body" "" (C8.unpack (rpBodyBytes rp))
                assertEqual "expected" ExpectOk expected
    ]
