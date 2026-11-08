module LspSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (isInfixOf)

import Hh200.Scanner as Hh
import Hh200.Types (Script, Snippet(..), SrcSpan(..))
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Lex and parse an inline script into its indexed 'Script'.
parseScript :: String -> IO Script
parseScript input = do
    res <- Hh.analyze (Snippet (L8.pack input))
    case res of
        Right s -> pure s
        Left err -> assertFailure ("parse failed: " ++ err)

spec :: TestTree
spec = testGroup "LSP states"
  [ testLSP_state
  , testLSP_scanSafe
  , testLSP_complete
  , testLSP_scanComplex
  , testLSP_context
  , testLSP_hover
  , testLSP_diagnostics
  , testLSP_symbols
  , testLSP_definition
  , testLSP_references
  , testLSP_formatting
  ]

testLSP_state :: TestTree
testLSP_state = testCase "lexer state" $ do
    let input = "GET /"
        action :: Hh.Alex (Hh.Token, Int)
        action = do
            Hh.alexSetUserState (Hh.AlexUserState { Hh.usCount = 42 })
            t <- Hh.alexMonadScan
            us <- Hh.alexGetUserState
            return (t, Hh.usCount us)

    case Hh.runAlex input action of
        Right (tok, count) -> do
            assertEqual "State check" 42 count
            case tok of
               Hh.METHOD _ "GET" -> pure ()
               _ -> assertFailure $ "Unexpected token: " ++ show tok
        Left err -> assertFailure $ "Lexer error: " ++ err

testLSP_scanSafe :: TestTree
testLSP_scanSafe = testGroup "LSP safe scan"
    [ testCase "valid input" $ do
        let input = "GET /"
        case Hh.scanSafe input of
            Right toks -> do
                assertBool "Not empty" (not (null toks))
                case head toks of
                    Hh.METHOD _ "GET" -> pure ()
                    _ -> assertFailure "First token should be GET"
            Left err -> assertFailure ("Scan failed: " ++ err)
    
    , testCase "recoverable error" $ do
        let input = "GET / @" -- @ is invalid
        case Hh.scanSafe input of
             Right toks -> assertFailure "Expected lex error but got tokens"
             Left err -> assertBool "Error message is present" (not (null err))
    ]

testLSP_complete :: TestTree
testLSP_complete = testGroup "LSP completion" (map completionCase completionTable)

-- | (input, cursor position, expected suggestions, unexpected suggestions).
completionTable :: [(String, Int, [String], [String])]
completionTable =
    [ ("", 0, ["GET", "POST"], [])
    , ("G", 1, ["GET"], ["POST"])
    , ("PO", 2, ["POST"], [])
    , ("GET / HT", 8, ["HTTP"], [])
    , ("D", 1, ["DELETE"], [])
    , ("g", 1, ["GET"], [])
    , ("po", 2, ["POST"], [])
    , ("O", 1, ["OPTIONS"], [])
    , ("HE", 2, ["HEAD"], [])
    , ("TR", 2, ["TRACE"], [])
    , ("CO", 2, ["CONNECT"], [])
    , ("Con", 3, ["Configs"], [])
    , ("Cap", 3, ["Captures"], [])
    , ("As", 2, ["Asserts"], [])
    , ("th", 2, ["then"], [])
    ]

completionCase :: (String, Int, [String], [String]) -> TestTree
completionCase (input, pos, yes, no) = testCase label $ do
    let suggs = Hh.complete input pos
    mapM_ (\s -> assertBool ("suggests " ++ show s ++ " for " ++ show input) (s `elem` suggs)) yes
    mapM_ (\s -> assertBool ("does not suggest " ++ show s ++ " for " ++ show input) (s `notElem` suggs)) no
  where
    label = "complete " ++ show input

testLSP_context :: TestTree
testLSP_context = testGroup "LSP context awareness"
    [ testCase "Inside Configs" $ do
        let input = "[Configs]\nuse"
        let suggs = Hh.complete input (length input)
        assertBool "Suggests use-tls" ("use-tls" `elem` suggs)
        assertBool "Does NOT suggest GET" ("GET" `notElem` suggs)

    , testCase "Top Level" $ do
        let input = "\n"
        let suggs = Hh.complete input (length input)
        assertBool "Suggests GET" ("GET" `elem` suggs)
        assertBool "Suggests Configs" ("Configs" `elem` suggs)
        assertBool "Does NOT suggest use-tls" ("use-tls" `notElem` suggs)

    , testCase "Switch Context" $ do
        let input = "[Configs]\nuse-tls: true\n[Captures]\n"
        let suggs = Hh.complete input (length input)
        assertBool "Does NOT suggest use-tls inside Captures" ("use-tls" `notElem` suggs)
        assertBool "Suggests GET" ("GET" `elem` suggs)

    , testCase "Suggest Captures" $ do
        let input = "GET http://a.com\n[Captures]\nmyVar: $.id\nGET http://b.com\nmy"
        let suggs = Hh.complete input (length input)
        assertBool "Suggests myVar" ("myVar" `elem` suggs)
    ]

testLSP_hover :: TestTree
testLSP_hover = testGroup "LSP hover"
    [ testCase "Hover GET" $ do
        let input = "GET /"
        let doc = Hh.hover input 1
        assertEqual "Hover GET" (Just "HTTP Method") doc

    , testCase "Hover use-tls" $ do
        let input = "[Configs]\nuse-tls: true"
        let doc = Hh.hover input 12
        assertEqual "Hover use-tls" (Just "Config: Toggle TLS") doc
        
    , testCase "Hover unknown" $ do
        let doc = Hh.hover "FOO" 1
        assertEqual "Hover unknown" Nothing doc
    ]

testLSP_diagnostics :: TestTree
testLSP_diagnostics = testGroup "LSP diagnostics"
  [ testCase "Diagnostic Parser Error" $ do
        let input = "GET"
        diags <- Hh.diagnostics input
        assertBool "Has diagnostics" (not (null diags))
        let ((line, _), msg) = head diags
        assertEqual "Error on line 1" 1 line
        assertBool "Message indicates parse error" ("Parse error" `isInfixOf` msg)
  ]

testLSP_symbols :: TestTree
testLSP_symbols = testGroup "LSP symbols"
    [ testCase "List requests" $ do
        let input = "GET http://example.com\nPOST http://example.com/post"
        let syms = Hh.documentSymbols input
        assertEqual "Symbol count" 2 (length syms)
        let (name1, (line1, _)) = head syms
        assertEqual "Symbol 1 name" "GET http://example.com" name1
        assertEqual "Symbol 1 line" 1 line1
        let (name2, (line2, _)) = syms !! 1
        assertEqual "Symbol 2 name" "POST http://example.com/post" name2
        assertEqual "Symbol 2 line" 2 line2

    , testCase "List requests and captures" $ do
        let input = "GET http://a.com\n[Captures]\nmyVar: $.id"
        let syms = Hh.documentSymbols input
        assertEqual "Symbol count" 2 (length syms)
        let (name2, (line2, _)) = syms !! 1
        assertEqual "Symbol 2 name" "myVar" name2
    ]

testLSP_definition :: TestTree
testLSP_definition = testGroup "LSP definition"
    [ testCase "Go to definition of capture" $ do
        script <- parseScript "GET http://example.com/api\n\nHTTP 200\n[Captures]\nmyVar: $.id\n\nGET http://example.com/{{myVar}}\n"
        -- cursor over the {{myVar}} usage on line 7 (1-based col 26)
        case Hh.definitionAt script (7, 26) of
            Just (SrcSpan (defLine, _) _) -> assertEqual "Definition line" 5 defLine
            Nothing -> assertFailure "Should have found definition"
    ]

testLSP_references :: TestTree
testLSP_references = testGroup "LSP references"
    [ testCase "Find references of capture" $ do
        script <- parseScript "GET http://example.com/api\n\nHTTP 200\n[Captures]\nmyVar: $.id\n\nGET http://example.com/{{myVar}}\n"
        -- cursor over the myVar definition on line 5
        let refs = Hh.referencesAt script (5, 1)
        assertEqual "References count" 2 (length refs)
        let lines = [ l | SrcSpan (l, _) _ <- refs ]
        assertBool "Contains definition line" (5 `elem` lines)
        assertBool "Contains usage line" (7 `elem` lines)
    ]

testLSP_formatting :: TestTree
testLSP_formatting = testGroup "LSP formatting"
    [ testCase "Format JSON body" $ do
        let input = "POST http://api.com\n{\"a\":1, \"b\":2}"
        let range = ((2, 1), (2, 16))
        let edits = Hh.formatRange input range
        
        assertEqual "Edits count" 1 (length edits)
        let (_, _, newText) = head edits
        -- Check exact formatting output: Aeson pretty print
        -- Normally formats to precisely something like: {\n    "a": 1,\n    "b": 2\n}
        assertEqual "Contains newlines" "{\n  \"a\":1,\n  \"b\":2\n}" newText
    ]

testLSP_scanComplex :: TestTree
testLSP_scanComplex = testCase "valid complex input" $ do
        let input = "POST /foo\n{\"a\": 1}"
        case Hh.scanSafe input of
            Right toks -> do
                let isPost (Hh.METHOD _ "POST") = True
                    isPost _ = False
                assertBool "Has POST" (any isPost toks)
            Left err -> assertFailure ("Scan failed: " ++ err)
