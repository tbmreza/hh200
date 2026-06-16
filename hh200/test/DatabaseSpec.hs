{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.ByteString as BS
import           System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import           System.Environment (setEnv)
import           System.FilePath ((</>))

import           Hh200.Database

spec :: TestTree
spec = testGroup "sqlite"
  [ testCase "init and close db" $ do
        let tmpDir = "test/tmp_db"
        createDirectoryIfMissing True tmpDir
        let dbPath = tmpDir </> "test.db"
        setEnv "HH200_SQLITE" dbPath
        conn <- initDb
        closeDb conn
        exists <- doesFileExist dbPath
        assertBool "database file should exist after initDb" exists
        removeDirectoryRecursive tmpDir
  ]
