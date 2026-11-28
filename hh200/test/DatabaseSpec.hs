{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The binary owns its schema: 'initDb' creates the tables on a new file,
-- records the version in @PRAGMA user_version@, applies later migrations in
-- order, and adopts a database Prisma created from @live/prisma@.
module DatabaseSpec (spec) where

import           Control.Exception (finally, try)
import           Data.Char (isSpace)
import           Data.Int (Int64)
import           Data.List (isPrefixOf, sort)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import           System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import           System.Environment (setEnv, unsetEnv)
import           System.FilePath ((</>))
import           System.IO (hClose, openTempFile)
import           System.Directory (removeFile)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Hh200.Database

spec :: TestTree
spec = testGroup "Database schema"
    [ testCase "a brand-new database file gets the schema and accepts a run" $
        withFreshPath $ \fp -> do
            conn <- initDb
            exists <- doesFileExist fp
            assertBool "initDb creates the file" exists
            v <- userVersion conn
            assertEqual "user_version records the schema version" schemaVersion v
            tables <- tableNames conn
            mapM_ (\t -> assertBool ("table " ++ t) (t `elem` tables))
                ["runs", "requests", "request_headers", "request_bodies"]
            rid <- roundTrip conn
            Sqlite.close conn
            assertEqual "first run id" 1 rid

    , testCase "a missing parent directory of HH200_SQLITE is created" $ do
        tmp <- getTemporaryDirectory
        (marker, h) <- openTempFile tmp "hh200-dbdir"
        hClose h
        removeFile marker
        let dir = marker </> "nested"
        setEnv "HH200_SQLITE" (dir </> "hh200.sqlite")
        (do conn <- initDb
            v <- userVersion conn
            Sqlite.close conn
            assertEqual "schema version" schemaVersion v)
          `finally` (unsetEnv "HH200_SQLITE" >> removeDirectoryRecursive marker)

    , testCase "opening the database twice is harmless" $
        withFreshPath $ \_ -> do
            c1 <- initDb
            _ <- roundTrip c1
            Sqlite.close c1
            c2 <- initDb
            c3 <- initDb  -- while c2 is still open
            v <- userVersion c2
            assertEqual "version unchanged" schemaVersion v
            runs <- listRuns c3
            assertEqual "the first run survives" 1 (length runs)
            rid <- roundTrip c2
            assertEqual "a second run after reopening" 2 rid
            Sqlite.close c2
            Sqlite.close c3

    , testCase "a Prisma-created database (live/prisma migrations) is adopted and accepts a run" $
        withFreshPath $ \fp -> do
            prisma <- Sqlite.open fp
            applyPrismaMigrations prisma
            Sqlite.close prisma
            conn <- initDb
            v <- userVersion conn
            assertEqual "adopted at the current version" schemaVersion v
            _ <- roundTrip conn
            Sqlite.close conn

    , testCase "the binary's schema matches the Prisma migrations column for column" $ do
        ours <- withMemDb $ \c -> migrate c >> schemaShape c
        theirs <- withMemDb $ \c -> applyPrismaMigrations c >> schemaShape c
        assertEqual "tables, columns, types, nullability, defaults, keys, indexes" theirs ours

    , testCase "migrations apply in order, only past user_version, each once" $
        withMemDb $ \c -> do
            let m1 = ["CREATE TABLE t (a INTEGER)"]
                m2 = ["ALTER TABLE t ADD COLUMN b TEXT", "INSERT INTO t (a, b) VALUES (1, 'x')"]
            migrateWith [m1] c
            v1 <- userVersion c
            assertEqual "after m1" 1 v1
            -- An upgraded binary ships m2: only m2 runs.
            migrateWith [m1, m2] c
            migrateWith [m1, m2] c
            v2 <- userVersion c
            assertEqual "after m2" 2 v2
            rows <- Sqlite.query_ c "SELECT a, b FROM t" :: IO [(Int, T.Text)]
            assertEqual "m2 ran exactly once" [(1, "x")] rows

    , testCase "a failing migration leaves the database at the previous version" $
        withMemDb $ \c -> do
            let m1 = ["CREATE TABLE t (a INTEGER)"]
                bad = ["CREATE TABLE u (a INTEGER)", "THIS IS NOT SQL"]
            migrateWith [m1] c
            r <- tryAny (migrateWith [m1, bad] c)
            assertBool "the bad step throws" (either (const True) (const False) r)
            v <- userVersion c
            assertEqual "still at 1" 1 v
            tables <- tableNames c
            assertBool "the bad step's first statement was rolled back" ("u" `notElem` tables)

    , testCase "a database from a newer hh200 is left alone" $
        withMemDb $ \c -> do
            Sqlite.execute_ c (Sqlite.Query (T.pack ("PRAGMA user_version = " ++ show (schemaVersion + 5))))
            migrate c
            v <- userVersion c
            assertEqual "version not lowered" (schemaVersion + 5) v
    ]

-- | Insert a run and a captured request through the binary's own writers, then
-- read them back. Returns the run id.
roundTrip :: Sqlite.Connection -> IO Int64
roundTrip conn = do
    Right rid <- insertRun conn RunRow
        { runId = 0, runName = "fresh", runScriptPath = "/tmp/x.hhs", runStartedAt = 0
        , runEndedAt = ETStillRunning, runStatus = "running", runConcurrency = 1
        , runRateLimit = 0, runControlSocket = "/tmp/cs" }
    let row = RequestRow 0 rid 0 1700000000 (Just 1.5) "GET" "http://example.com/" (Just 200) Nothing (Just 5) (Just 0) (Just 0)
    r <- insertCapturedRequest conn CapturedRequest
        { capturedRow = row
        , capturedHeaders = [RequestHeaderRow "request" "Authorization" "[REDACTED]"]
        , capturedBodies = [RequestBodyRow "response" (Just "hello") False]
        }
    assertEqual "request captured" (Right ()) r
    Right got <- getRunById conn rid
    assertEqual "run read back" "fresh" (runName got)
    Right [detail] <- listRequestsByRun conn rid
    assertEqual "url" "http://example.com/" (requestUrl (detailRow detail))
    assertEqual "headers" 1 (length (detailHeaders detail))
    assertEqual "bodies" [Just "hello"] (map requestBodyFullContent (detailBodies detail))
    pure rid

-- | Run every @live/prisma/migrations/*/migration.sql@, in order. Their
-- statements contain no semicolons, so splitting on @;@ is enough.
applyPrismaMigrations :: Sqlite.Connection -> IO ()
applyPrismaMigrations c = do
    let root = "../live/prisma/migrations"
    dirs <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory root
    files <- filterM' doesFileExist [root </> d </> "migration.sql" | d <- dirs]
    assertBool "found Prisma migrations" (not (null files))
    mapM_ (\f -> readFile f >>= mapM_ (Sqlite.execute_ c . Sqlite.Query . T.pack) . statements) files
  where
    statements = filter (not . all isSpace) . splitOn ';' . unlines . filter (not . ("--" `isPrefixOf`)) . lines
    splitOn ch s = case break (== ch) s of
        (a, [])       -> [a]
        (a, _ : rest) -> a : splitOn ch rest
    filterM' p = fmap concat . mapM (\x -> (\b -> [x | b]) <$> p x)

-- | Everything about the schema that affects what the code can read and write.
schemaShape :: Sqlite.Connection -> IO [(String, [(T.Text, T.Text, Int, Maybe T.Text, Int)], [(T.Text, T.Text, T.Text)], [T.Text])]
schemaShape c = do
    tables <- sort <$> tableNames c
    mapM (\t -> do
        cols <- Sqlite.query_ c (q ("PRAGMA table_info(" ++ show t ++ ")"))
        fks  <- Sqlite.query_ c (q ("PRAGMA foreign_key_list(" ++ show t ++ ")"))
        idx  <- Sqlite.query_ c (q ("PRAGMA index_list(" ++ show t ++ ")"))
        pure ( t
             , [ (n, T.toUpper ty, nn, normDefault d, pk) | (_ :: Int, n, ty, nn, d, pk) <- cols ]
             , sort [ (tbl, from, to) | (_ :: Int, _ :: Int, tbl, from, to, _ :: T.Text, _ :: T.Text, _ :: T.Text) <- fks ]
             , sort [ n | (_ :: Int, n, _ :: Int, _ :: T.Text, _ :: Int) <- idx ]
             )) tables
  where
    q = Sqlite.Query . T.pack
    normDefault = fmap T.toLower

tableNames :: Sqlite.Connection -> IO [String]
tableNames c = map (T.unpack . Sqlite.fromOnly) <$>
    Sqlite.query_ c "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"

-- | Point @HH200_SQLITE@ at a path that doesn't exist yet.
withFreshPath :: (FilePath -> IO a) -> IO a
withFreshPath act = do
    tmp <- getTemporaryDirectory
    (marker, h) <- openTempFile tmp "hh200-fresh"
    hClose h
    removeFile marker
    createDirectory marker
    let fp = marker </> "hh200.sqlite"
    setEnv "HH200_SQLITE" fp
    act fp `finally` (unsetEnv "HH200_SQLITE" >> removeDirectoryRecursive marker)

withMemDb :: (Sqlite.Connection -> IO a) -> IO a
withMemDb act = do
    c <- Sqlite.open ":memory:"
    act c `finally` Sqlite.close c

tryAny :: IO a -> IO (Either Sqlite.SQLError a)
tryAny = try
