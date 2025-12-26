module BlindLsp where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (isInfixOf)

import Hh200.Scanner as Hh

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
  , testLSP_rename
  , testLSP_formatting
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
        -- Even with bad input, maybe we want some tokens?
        -- For now, let's just ensure it doesn't crash (which alexScanTokens does)
        let input = "GET / @" -- @ is invalid
        case Hh.scanSafe input of
             Right _ -> pure () -- If it recovers, great
             Left _ -> pure ()  -- If it returns Left, that's also fine (safe)
    ]

testLSP_complete :: TestTree
testLSP_complete = testGroup "LSP completion"
    [ testCase "empty input" $ do
        let suggs = Hh.complete "" 0
        assertBool "Suggests GET" ("GET" `elem` suggs)
        assertBool "Suggests POST" ("POST" `elem` suggs)
    
    , testCase "partial method G" $ do
        let suggs = Hh.complete "G" 1
        assertBool "Suggests GET" ("GET" `elem` suggs)
        assertBool "Does not suggest POST" ("POST" `notElem` suggs)

    , testCase "partial method PO" $ do
        let suggs = Hh.complete "PO" 2
        assertBool "Suggests POST" ("POST" `elem` suggs)
        
    , testCase "partial HTTP keyword" $ do
        let suggs = Hh.complete "GET / HT" 8
        assertBool "Suggests HTTP" ("HTTP" `elem` suggs)

    , testCase "partial method D" $ do
        let suggs = Hh.complete "D" 1
        assertBool "Suggests DELETE" ("DELETE" `elem` suggs)

    , testCase "lowercase partial method g" $ do
        let suggs = Hh.complete "g" 1
        assertBool "Suggests GET" ("GET" `elem` suggs)

    , testCase "lowercase partial method po" $ do
        let suggs = Hh.complete "po" 2
        assertBool "Suggests POST" ("POST" `elem` suggs)
        
    , testCase "partial method O" $ do
        let suggs = Hh.complete "O" 1
        assertBool "Suggests OPTIONS" ("OPTIONS" `elem` suggs)
        
    , testCase "partial method HE" $ do
        let suggs = Hh.complete "HE" 2
        assertBool "Suggests HEAD" ("HEAD" `elem` suggs)

    , testCase "keyword Configs" $ do
        let suggs = Hh.complete "Con" 3
        assertBool "Suggests Configs" ("Configs" `elem` suggs)

    , testCase "keyword Captures" $ do
        let suggs = Hh.complete "Cap" 3
        assertBool "Suggests Captures" ("Captures" `elem` suggs)

    , testCase "keyword Asserts" $ do
        let suggs = Hh.complete "As" 2
        assertBool "Suggests Asserts" ("Asserts" `elem` suggs)

    , testCase "keyword then" $ do
        let suggs = Hh.complete "th" 2
        assertBool "Suggests then" ("then" `elem` suggs)
    ]

testLSP_context :: TestTree
testLSP_context = testGroup "LSP context awareness"
    [ testCase "Inside Configs" $ do
        -- Simulate being inside [Configs] block
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
        -- "GET" is at 0-3. Hover at 1.
        let doc = Hh.hover input 1
        assertEqual "Hover GET" (Just "HTTP Method") doc

    , testCase "Hover use-tls" $ do
        let input = "[Configs]\nuse-tls: true"
        -- [Configs] is 0-9. \n is 9. use-tls is 10-17.
        -- Hover at 12 (somewhere in "use-tls")
        let doc = Hh.hover input 12
        assertEqual "Hover use-tls" (Just "Config: Toggle TLS") doc
        
    , testCase "Hover unknown" $ do
        let doc = Hh.hover "FOO" 1
        assertEqual "Hover unknown" Nothing doc
    ]

testLSP_diagnostics :: TestTree
testLSP_diagnostics = testGroup "LSP diagnostics"
    [ testCase "Diagnostic Parser Error" $ do
        let input = "GET" -- Missing URL, invalid grammar
        let diags = Hh.diagnostics input
        assertBool "Has diagnostics" (not (null diags))
        -- Expect error. "Parse error..."
        let ((line, _), msg) = head diags
        -- Line 1.
        assertEqual "Error on line 1" 1 line
        assertBool "Message indicates parse error" ("Parse error" `isInfixOf` msg)
    ]

testLSP_symbols :: TestTree
testLSP_symbols = testGroup "LSP symbols"
    [ testCase "List requests" $ do
        let input = "GET http://example.com\nPOST http://example.com/post"
        let syms = Hh.documentSymbols input
        -- Expect 2 symbols
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
        -- GET and myVar
        assertEqual "Symbol count" 2 (length syms)
        let (name2, (line2, _)) = syms !! 1
        assertEqual "Symbol 2 name" "myVar" name2
    ]

testLSP_definition :: TestTree
testLSP_definition = testGroup "LSP definition"
    [ testCase "Go to definition (stub)" $ do
        let loc = Hh.definition "GET" 1
        assertEqual "Definition stub" Nothing loc

    , testCase "Go to definition of capture" $ do
        let input = "[Captures]\nmyVar: $.id\nGET http://example.com/{{myVar}}"
        -- myVar definition is at line 2.
        -- Usage is at the end.
        -- "GET http://example.com/{{myVar}}"
        -- [Captures] (len 10+1)
        -- myVar: $.id (len 11+1)
        -- GET ... (len 4)
        -- http://...{{ (len ?)
        
        -- Let's calculate position of usage "myVar" roughly.
        -- Input length is around 60.
        let pos = length input - 4 -- Inside "myVar"
        
        let res = Hh.definition input pos
        
        case res of
            Just ((defLine, _), _) -> assertEqual "Definition line" 2 defLine
            Nothing -> assertFailure "Should have found definition"
    ]

testLSP_references :: TestTree
testLSP_references = testGroup "LSP references"
    [ testCase "Find references of capture" $ do
        let input = "[Captures]\nmyVar: $.id\nGET http://example.com/{{myVar}}"
        -- myVar defined at line 2.
        -- Used at line 3.
        -- Invoke on definition (line 2).
        -- Input:
        -- [Captures]\n (11 chars)
        -- myVar (5 chars)
        let pos = 13 -- inside "myVar"
        let refs = Hh.references input pos
        
        assertEqual "References count" 2 (length refs)
        let lines = map (fst . fst) refs
        assertBool "Contains line 2" (2 `elem` lines)
        assertBool "Contains line 3" (3 `elem` lines)
    ]

testLSP_rename :: TestTree
testLSP_rename = testGroup "LSP rename"
    [ testCase "Rename capture" $ do
        let input = "[Captures]\nvar1: $.id\nGET http://site.com/{{var1}}"
        -- Rename var1 to newVar.
        -- Cursor at definition (line 2).
        let pos = 13 -- inside "var1"
        let newName = "newVar"
        
        let edits = Hh.rename input pos newName
        -- Expect 2 edits.
        assertEqual "Edits count" 2 (length edits)
        
        -- Apply edits (naive application assuming no overlap and sorted? No, standard LSP applies all)
        -- Let's just check the content of edits.
        
        -- Edit 1: line 2, replace "var1" with "newVar"
        let ((l1, c1), (l1', c1'), txt1) = head edits
        assertEqual "Edit 1 line" 2 l1
        assertEqual "Edit 1 text" newName txt1
        
        -- Edit 2: line 3, replace "var1" with "newVar"
        let ((l2, c2), (l2', c2'), txt2) = edits !! 1
        assertEqual "Edit 2 line" 3 l2
        assertEqual "Edit 2 text" newName txt2
    ]

testLSP_formatting :: TestTree
testLSP_formatting = testGroup "LSP formatting"
    [ testCase "Format JSON body" $ do
        let input = "POST http://api.com\n{\"a\":1, \"b\":2}"
        -- Range covers the body: line 2
        let range = ((2, 1), (2, 16)) -- "{\"a\":1, \"b\":2}"
        let edits = Hh.formatRange input range
        
        assertEqual "Edits count" 1 (length edits)
        let (_, _, newText) = head edits
        -- Expect pretty printed JSON? Or just simple spacing?
        -- Let's implement simple indent or spacing for now.
        -- "{\n  \"a\": 1,\n  \"b\": 2\n}"
        assertBool "Contains newlines" ('\n' `elem` newText)
        assertBool "Contains indentation" ("  \"a\"" `isInfixOf` newText)
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

