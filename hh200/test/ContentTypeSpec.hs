{-# LANGUAGE OverloadedStrings #-}

module ContentTypeSpec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import Hh200.ContentType
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import qualified Data.ByteString as BS
import System.FilePath ((</>))

spec :: TestTree
spec = testGroup "ContentType"
    [ testExtensionMapping
    , testSaveBody
    ]

testExtensionMapping :: TestTree
testExtensionMapping = testCase "extensionFor maps MIME types to extensions" $ do
    assertEqual "pdf" "pdf" (extensionFor "application/pdf")
    assertEqual "audio" "mp3" (extensionFor "audio/mpeg")
    assertEqual "gif" "gif" (extensionFor "image/gif")
    assertEqual "multipart" "txt" (extensionFor "multipart/form-data")
    assertEqual "csv" "csv" (extensionFor "text/csv")
    assertEqual "mp4" "mp4" (extensionFor "video/mp4")
    assertEqual "xlsx" "xlsx" (extensionFor "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    assertEqual "json" "json" (extensionFor "application/json")
    assertEqual "fallback" "dat" (extensionFor "application/unknown-stuff")
    -- Test with parameters
    assertEqual "csv with charset" "csv" (extensionFor "text/csv; charset=utf-8")

testSaveBody :: TestTree
testSaveBody = testCase "saveBody writes file with correct extension" $ do
    let dir = "test/tmp_content_type"
    createDirectoryIfMissing True dir
    
    let content = "some content"
    path <- saveBody dir "testfile" "text/csv" content
    
    exists <- doesFileExist path
    assertBool "File should exist" exists
    assertEqual "Filename should have csv extension" (dir </> "testfile.csv") path
    
    readContent <- BS.readFile path
    assertEqual "Content should match" content readContent
    
    removeDirectoryRecursive dir
