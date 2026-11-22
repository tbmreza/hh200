{-# LANGUAGE OverloadedStrings #-}

-- | The curl front end ("Hh200.Curl"): shell-subset lexing and parsing, the
-- constructs it refuses, lowering onto a 'CallItem', and @--emit-hhs@.
module CurlFrontendSpec (spec, resendSnippet) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict as HM
import           Data.List (isInfixOf)
import qualified Data.Text as Text

import qualified BEL
import           Hh200.Curl
import           Hh200.Scanner (analyze)
import           Hh200.Types

-- | The snippet from AGENTS.md, as a user would paste it.
resendSnippet :: String -> String
resendSnippet url = unlines
    [ "IMG_BASE64=$(base64 -w 0 ./photo.png)"
    , "curl -X POST '" ++ url ++ "' \\"
    , "  -H 'Authorization: Bearer re_YOUR_API_KEY' \\"
    , "  -H 'Content-Type: application/json' \\"
    , "  --data-binary @- <<EOF"
    , "{"
    , "  \"from\": \"sender@example.com\","
    , "  \"to\": [\"recipient@example.com\"],"
    , "  \"subject\": \"Email with local image\","
    , "  \"html\": \"<p>See attached image.</p>\","
    , "  \"attachments\": ["
    , "    {"
    , "      \"filename\": \"photo.png\","
    , "      \"content\": \"$IMG_BASE64\""
    , "    }"
    , "  ]"
    , "}"
    , "EOF"
    ]

spec :: TestTree
spec = testGroup "curl front end (Hh200.Curl)"
    [ sharedTests
    , parseTests
    , rejectTests
    , lowerTests
    , emitTests
    ]

-- ---------------------------------------------------------------------------
-- Shared with the conformance harness
-- ---------------------------------------------------------------------------

sharedTests :: TestTree
sharedTests = testGroup "shared helpers"
    [ testCase "shellWords: quoting and escapes" $ do
        assertEqual "double quotes" ["curl", "-X", "PUT", "http://e/a b"] (shellWords "curl -X PUT \"http://e/a b\"")
        assertEqual "single quotes" ["-H", "A: b c"] (shellWords "-H 'A: b c'")
        assertEqual "adjacent quoted parts join" ["ab c"] (shellWords "\"a\"'b c'")
        assertEqual "backslash escape" ["a b"] (shellWords "a\\ b")
        assertEqual "escaped quote in double quotes" ["say \"hi\""] (shellWords "\"say \\\"hi\\\"\"")
    , testCase "shellWords reads the vendored curl commands like the old tokenizer" $ do
        -- The five <command>s under test/curl/data, and the tokens the
        -- previous whitespace-and-double-quote tokenizer produced for them.
        assertEqual "test1" ["http://%HOSTIP:%HTTPPORT/%TESTNUMBER"]
            (shellWords "http://%HOSTIP:%HTTPPORT/%TESTNUMBER")
        assertEqual "test152" ["http://%HOSTIP:%HTTPPORT/%TESTNUMBER", "--fail"]
            (shellWords "http://%HOSTIP:%HTTPPORT/%TESTNUMBER --fail")
        assertEqual "test30" ["http://%HOSTIP:%HTTPPORT/want/%TESTNUMBER"]
            (shellWords "http://%HOSTIP:%HTTPPORT/want/%TESTNUMBER")
        assertEqual "test59" ["http://%HOSTIP:%HTTPPORT?mooo/%TESTNUMBER"]
            (shellWords "\"http://%HOSTIP:%HTTPPORT?mooo/%TESTNUMBER\"")
        assertEqual "test98"
            ["-T", "-", "-H", "Transfer-Encoding:", "-H", "Content-Length: 14", "http://%HOSTIP:%HTTPPORT/%TESTNUMBER"]
            (shellWords "-T - -H \"Transfer-Encoding:\" -H \"Content-Length: 14\" http://%HOSTIP:%HTTPPORT/%TESTNUMBER")
    , testCase "curlMethod follows curl's precedence" $ do
        assertEqual "default" "GET" (curlMethod ["http://x/"])
        assertEqual "-X" "DELETE" (curlMethod ["-X", "DELETE", "-d", "a"])
        assertEqual "-X attached" "PATCH" (curlMethod ["-XPATCH", "http://x/"])
        assertEqual "--request=" "PUT" (curlMethod ["--request=PUT"])
        assertEqual "-T" "PUT" (curlMethod ["-T", "f"])
        assertEqual "--data-raw implies POST" "POST" (curlMethod ["--data-raw", "x"])
        assertEqual "--json implies POST" "POST" (curlMethod ["--json", "{}"])
    , testCase "curlDataArg takes the first data flag's value" $ do
        assertEqual "-d" (Just "a=1") (curlDataArg ["-d", "a=1", "-d", "b=2"])
        assertEqual "none" Nothing (curlDataArg ["http://x/"])
    ]

-- ---------------------------------------------------------------------------
-- Parsing the accepted subset
-- ---------------------------------------------------------------------------

parsed :: String -> IO CurlSnippet
parsed src = either (\d -> assertFailure ("unexpected diagnostic: " ++ show d) >> fail "unreachable") pure
    (parseCurlSnippet src)

parseTests :: TestTree
parseTests = testGroup "accepted subset"
    [ testCase "the Resend snippet" $ do
        CurlSnippet assigns req <- parsed (resendSnippet "https://api.resend.com/emails")
        assertEqual "assignment" [Assignment "IMG_BASE64" (AssignBase64File "./photo.png")] assigns
        assertEqual "method" "POST" (crMethod req)
        assertEqual "url" [Lit "https://api.resend.com/emails"] (crUrl req)
        assertEqual "headers"
            [ ("Authorization", [Lit "Bearer re_YOUR_API_KEY"])
            , ("Content-Type", [Lit "application/json"]) ]
            (crHeaders req)
        case crData req of
            [DataStdin ps] -> do
                assertBool "refers to IMG_BASE64" (Ref "IMG_BASE64" `elem` ps)
                assertBool "keeps the JSON text" (any (\p -> case p of Lit t -> "\"filename\": \"photo.png\"" `isInfixOf` t; _ -> False) ps)
            other -> assertFailure ("expected one stdin body, got " ++ show other)
    , testCase "base64 variants become a base64 file" $
        mapM_ (\(src, path) -> do
                CurlSnippet assigns _ <- parsed (src ++ "\ncurl http://x/\n")
                assertEqual src [Assignment "A" (AssignBase64File path)] assigns)
            [ ("A=$(base64 -w0 p.png)", "p.png")
            , ("A=$(base64 --wrap=0 p.png)", "p.png")
            , ("A=$(base64 -i p.png)", "p.png")
            , ("A=$(base64 < p.png)", "p.png")
            , ("A=\"$(base64 -w 0 'my photo.png')\"", "my photo.png")
            , ("export A=$(base64 -w 0 p.png)", "p.png") ]
    , testCase "references: $NAME, ${NAME}, in double quotes, not in single quotes" $ do
        CurlSnippet _ req <- parsed "curl -H \"X: $A-${B}\" -H 'Y: $C' http://x/\n"
        assertEqual "headers"
            [ ("X", [Ref "A", Lit "-", Ref "B"]), ("Y", [Lit "$C"]) ]
            (crHeaders req)
    , testCase "a quoted heredoc delimiter turns off expansion" $ do
        CurlSnippet _ req <- parsed "curl --data-binary @- http://x/ <<'EOF'\n{\"a\": \"$X\"}\nEOF\n"
        assertEqual "body" [DataStdin [Lit "{\"a\": \"$X\"}\n"]] (crData req)
    , testCase "<<- strips leading tabs, including on the delimiter" $ do
        CurlSnippet _ req <- parsed "curl -d @- http://x/ <<-EOF\n\t{\"a\": 1}\n\tEOF\n"
        assertEqual "body" [DataStdin [Lit "{\"a\": 1}\n"]] (crData req)
    , testCase "comments, blank lines, CRLF and continuations are fine" $ do
        CurlSnippet _ req <- parsed "# send it\r\n\r\ncurl \\\r\n  -s -S \\\r\n  http://x/a # trailing\r\n"
        assertEqual "url" [Lit "http://x/a"] (crUrl req)
        assertEqual "method" "GET" (crMethod req)
    , testCase "flag spellings" $ do
        CurlSnippet _ req <- parsed "curl --request=PUT --header='A: 1' --data-raw '@x' --url http://x/\n"
        assertEqual "method" "PUT" (crMethod req)
        assertEqual "header" [("A", [Lit "1"])] (crHeaders req)
        assertEqual "--data-raw keeps a leading @" [DataPieces [Lit "@x"]] (crData req)
        assertEqual "url" [Lit "http://x/"] (crUrl req)
    , testCase "ignored flags, including combined short ones" $ do
        CurlSnippet _ req <- parsed "curl -sSL -v --fail http://x/\n"
        assertEqual "url" [Lit "http://x/"] (crUrl req)
    , testCase "a URL without a scheme gets http://, as curl does" $ do
        CurlSnippet _ req <- parsed "curl example.com/a\n"
        assertEqual "url" [Lit "http://example.com/a"] (crUrl req)
    ]

-- ---------------------------------------------------------------------------
-- Refusals: each is a positioned diagnostic
-- ---------------------------------------------------------------------------

rejects :: String -> (Int, Int) -> String -> TestTree
rejects src pos needle = testCase (show src) $
    case parseCurlSnippet src of
        Right s -> assertFailure ("accepted: " ++ show s)
        Left d -> do
            assertEqual ("position of: " ++ diagMessage d) pos (spanStart (diagSpan d))
            assertBool ("message " ++ show (diagMessage d) ++ " should mention " ++ show needle)
                (needle `isInfixOf` diagMessage d)

rejectTests :: TestTree
rejectTests = testGroup "refused, with a position"
    [ rejects "curl http://x/ | jq .\n" (1, 16) "pipes"
    , rejects "A=1; curl http://x/\n" (1, 4) "';'"
    , rejects "A=1 && curl http://x/\n" (1, 5) "'&&'"
    , rejects "curl http://x/ > out.json\n" (1, 16) "redirection"
    , rejects "A=`cat f`\ncurl http://x/\n" (1, 3) "backtick"
    , rejects "A=$(cat secrets)\ncurl http://x/\n" (1, 3) "only $(base64 -w 0 PATH)"
    , rejects "A=$(base64 p.png)\ncurl http://x/\n" (1, 3) "-w 0"
    , rejects "A=x$(base64 -w0 p)\ncurl http://x/\n" (1, 4) "whole value"
    , rejects "curl http://x/ -H \"X: $(date)\"\n" (1, 23) "curl arguments"
    , rejects "echo hi\ncurl http://x/\n" (1, 1) "unsupported command 'echo'"
    , rejects "A=1 curl http://x/\n" (1, 5) "own line"
    , rejects "curl http://x/\nA=1\n" (2, 1) "after the curl"
    , rejects "curl http://a/\ncurl http://b/\n" (2, 1) "only one curl"
    , rejects "A=1\n" (1, 1) "no curl command"
    , rejects "curl -F 'f=@p.png' http://x/\n" (1, 6) "unsupported curl flag -F"
    , rejects "curl -d @body.json http://x/\n" (1, 6) "@FILE"
    , rejects "curl -d @- http://x/\n" (1, 1) "needs a heredoc"
    , rejects "curl http://x/ <<EOF\n{}\nEOF\n" (1, 1) "only read as the body"
    , rejects "curl -d @- http://x/ <<EOF\n{}\n" (1, 22) "no closing delimiter"
    , rejects "curl -H 'X: a' -H 'x: b' http://x/\n" (1, 16) "given twice"
    , rejects "curl -H 'Transfer-Encoding:' http://x/\n" (1, 6) "empty header value"
    , rejects "curl -H \"X: ${A:-d}\" http://x/\n" (1, 13) "${NAME}"
    , rejects "curl 'http://x/\n" (1, 6) "unterminated single quote"
    , rejects "curl http://a/ http://b/\n" (1, 16) "more than one URL"
    , rejects "cat <<EOF\nx\nEOF\n" (1, 1) "unsupported command 'cat'"
    ]

-- ---------------------------------------------------------------------------
-- Lowering onto a CallItem
-- ---------------------------------------------------------------------------

lowered :: String -> IO (CallItem, [String])
lowered src = case parseCurlSnippet src of
    Left d -> assertFailure ("diagnostic: " ++ show d) >> fail "unreachable"
    Right s -> either (\e -> assertFailure ("lowering: " ++ e) >> fail "unreachable") pure (lowerCurl s)

headerOf :: String -> CallItem -> Maybe [BEL.Part]
headerOf n ci = let RhsDict d = rqHeaders (ciRequestSpec ci) in HM.lookup (Text.pack n) d

varsOf :: CallItem -> HM.HashMap String [BEL.Part]
varsOf ci = case sqVars (rqSquares (ciRequestSpec ci)) of
    Just (RequestSquareVars (RhsDict d)) -> HM.fromList [ (Text.unpack k, v) | (k, v) <- HM.toList d ]
    _ -> HM.empty

lowerTests :: TestTree
lowerTests = testGroup "lowering"
    [ testCase "the Resend snippet lowers to [Vars] + a templated body" $ do
        (ci, warnings) <- lowered (resendSnippet "https://api.resend.com/emails")
        let rs = ciRequestSpec ci
        assertEqual "method" "POST" (rqMethod rs)
        case rqUrl rs of
            LexedUrlFull u -> assertEqual "url" "https://api.resend.com/emails" u
            other -> assertFailure ("url: " ++ show other)
        assertEqual "vars" (HM.fromList [("IMG_BASE64", [BEL.R "base64-file,./photo.png"])]) (varsOf ci)
        assertBool "body template" ("\"content\": \"{{IMG_BASE64}}\"" `isInfixOf` rqBody rs)
        assertEqual "content type kept, not defaulted" (Just [BEL.R "application/json"]) (headerOf "Content-Type" ci)
        assertBool "warns about the literal key" (any ("Authorization" `isInfixOf`) warnings)
    , testCase "a $NAME credential doesn't warn and becomes {{NAME}}" $ do
        (ci, warnings) <- lowered "curl -H \"Authorization: Bearer $KEY\" http://x/\n"
        assertEqual "warnings" [] warnings
        assertEqual "header" (Just [BEL.R "Bearer ", BEL.L "KEY"]) (headerOf "Authorization" ci)
    , testCase "-d without a Content-Type gets curl's form default" $ do
        (ci, _) <- lowered "curl -d 'a=1' -d \"b=$B\" http://x/\n"
        assertEqual "method" "POST" (rqMethod (ciRequestSpec ci))
        assertEqual "content type" (Just [BEL.R "application/x-www-form-urlencoded"]) (headerOf "Content-Type" ci)
        assertEqual "data joined with &" "a=1&b={{B}}" (rqBody (ciRequestSpec ci))
    , testCase "--json sets Content-Type and Accept" $ do
        (ci, _) <- lowered "curl --json '{\"a\": 1}' http://x/\n"
        assertEqual "content type" (Just [BEL.R "application/json"]) (headerOf "Content-Type" ci)
        assertEqual "accept" (Just [BEL.R "application/json"]) (headerOf "Accept" ci)
    , testCase "a $NAME in the URL makes a URL template" $ do
        (ci, _) <- lowered "curl \"$BASE/emails\"\n"
        case rqUrl (ciRequestSpec ci) of
            LexedUrlSegments ps -> assertEqual "parts" [BEL.L "BASE", BEL.R "/emails"] ps
            other -> assertFailure ("url: " ++ show other)
    , testCase "an assignment using an earlier literal one is inlined" $ do
        (ci, _) <- lowered "HOST=api.example.com\nURL=\"https://$HOST/v1\"\ncurl \"$URL\"\n"
        assertEqual "URL var" (Just [BEL.R "https://api.example.com/v1"]) (HM.lookup "URL" (varsOf ci))
    , testCase "an assignment using an external name keeps the reference" $ do
        (ci, _) <- lowered "AUTH=\"Bearer $KEY\"\ncurl -H \"Authorization: $AUTH\" http://x/\n"
        assertEqual "AUTH var" (Just [BEL.R "Bearer ", BEL.L "KEY"]) (HM.lookup "AUTH" (varsOf ci))
    , testCase "a literal {{ is refused rather than read as a template" $
        case parseCurlSnippet "curl -d '{{x}}' http://x/\n" of
            Left d -> assertFailure (show d)
            Right s -> assertBool "refused" (either ("'{{'" `isInfixOf`) (const False) (lowerCurl s))
    ]

-- ---------------------------------------------------------------------------
-- --emit-hhs
-- ---------------------------------------------------------------------------

emitTests :: TestTree
emitTests = testGroup "--emit-hhs"
    [ testCase "the Resend snippet emits .hhs that parses back to the same request" $ do
        (ci, _) <- lowered (resendSnippet "https://api.resend.com/emails")
        text <- either (\e -> assertFailure e >> fail "unreachable") pure (emitHhs ci)
        r <- analyze (Snippet (L8.pack text))
        case r of
            Left err -> assertFailure ("emitted .hhs doesn't parse: " ++ err ++ "\n" ++ text)
            Right script -> case callItems script of
                [back] -> do
                    let a = ciRequestSpec ci
                        b = ciRequestSpec back
                    assertEqual "method" (rqMethod a) (rqMethod b)
                    assertEqual "vars" (varsOf ci) (varsOf back)
                    assertEqual "headers" (rqHeaders a) (rqHeaders b)
                    assertEqual "body" (trim (rqBody a)) (trim (rqBody b))
                other -> assertFailure ("expected one call item, got " ++ show (length other))
    , testCase "a form body can't be emitted" $ do
        (ci, _) <- lowered "curl -d 'a=1' http://x/\n"
        assertBool "refused" (either ("JSON object" `isInfixOf`) (const False) (emitHhs ci))
    , testCase "a URL that doesn't start with http can't be emitted" $ do
        (ci, _) <- lowered "curl \"$BASE/x\"\n"
        assertBool "refused" (either ("http" `isInfixOf`) (const False) (emitHhs ci))
    ]
  where
    trim = reverse . dropWhile (`elem` (" \n" :: String)) . reverse . dropWhile (`elem` (" \n" :: String))
