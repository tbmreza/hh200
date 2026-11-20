{-# LANGUAGE OverloadedStrings #-}

-- | Behaviour of @package-release.sh@ that any fix to it must preserve:
-- version argument handling, the tarball layout (binary + README), and a
-- checksum that verifies. @stack@ is replaced with a stub on @PATH@ so the
-- script packages a fake binary without a real build.
module ReleaseScriptSpec (spec) where

import           Control.Exception (bracket)
import           Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import           System.Directory
import           System.Environment (getEnvironment)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

spec :: TestTree
spec = testGroup "package-release.sh"
  [ testMissingVersion
  , testVersionFromEnv
  , testPackagesBinaryAndReadme
  , testChecksumVerifies
  ]

-- | A scratch workspace: @work/@ is the cwd the script runs in (with a
-- README.md), @install/bin/hh200@ is the fake built binary, and @fakebin/stack@
-- answers @stack path --local-install-root@.
data Ws = Ws { wsWork :: FilePath, wsEnv :: [(String, String)], wsScript :: FilePath }

withWorkspace :: (Ws -> IO a) -> IO a
withWorkspace act = do
    script <- makeAbsolute "package-release.sh"
    tmp <- getTemporaryDirectory
    bracket (mkFresh (tmp </> "hh200-release-spec")) removeDirectoryRecursive $ \root -> do
        let work = root </> "work"
            install = root </> "install"
            fakebin = root </> "fakebin"
        mapM_ (createDirectoryIfMissing True) [work, install </> "bin", fakebin]
        writeFile (work </> "README.md") "readme\n"
        writeFile (install </> "bin" </> "hh200") "#!/bin/sh\necho fake hh200\n"
        makeExecutable (install </> "bin" </> "hh200")
        writeFile (fakebin </> "stack") ("#!/bin/sh\necho '" ++ install ++ "'\n")
        makeExecutable (fakebin </> "stack")
        env0 <- getEnvironment
        let path = maybe fakebin (\p -> fakebin ++ ":" ++ p) (lookup "PATH" env0)
            env = ("PATH", path) : filter (\(k, _) -> k /= "PATH" && k /= "HH200_VERSION") env0
        act (Ws work env script)
  where
    mkFresh base = go (0 :: Int)
      where
        go n = do
            let d = base ++ "-" ++ show n
            exists <- doesPathExist d
            if exists then go (n + 1) else createDirectoryIfMissing True d >> pure d
    makeExecutable f = getPermissions f >>= setPermissions f . setOwnerExecutable True

runScript :: Ws -> [(String, String)] -> [String] -> IO (ExitCode, String, String)
runScript ws extraEnv args =
    readCreateProcessWithExitCode
        (proc "bash" (wsScript ws : args)) { cwd = Just (wsWork ws), env = Just (extraEnv ++ wsEnv ws) }
        ""

-- | The single release tarball the script produced for @version@.
findTarball :: Ws -> String -> IO FilePath
findTarball ws version = do
    let dist = wsWork ws </> "dist"
    entries <- listDirectory dist
    case [ e | e <- entries, ("hh200-" ++ version ++ "-") `isPrefixOf` e, ".tar.gz" `isSuffixOf` e ] of
        [one] -> pure (dist </> one)
        other -> assertFailure ("expected exactly one tarball in dist/, got " ++ show other) >> pure ""

testMissingVersion :: TestTree
testMissingVersion = testCase "no version: exits non-zero with usage" $ withWorkspace $ \ws -> do
    (code, _, err) <- runScript ws [] []
    assertBool "non-zero exit" (code /= ExitSuccess)
    assertBool ("usage on stderr: " ++ err) ("Usage" `isInfixOf` err)

testVersionFromEnv :: TestTree
testVersionFromEnv = testCase "HH200_VERSION is honoured when no argument is given" $ withWorkspace $ \ws -> do
    (code, out, err) <- runScript ws [("HH200_VERSION", "v7.7.7")] []
    assertEqual ("exit (stderr: " ++ err ++ ")") ExitSuccess code
    _ <- findTarball ws "v7.7.7"
    assertBool "mentions version" ("v7.7.7" `isInfixOf` out)

testPackagesBinaryAndReadme :: TestTree
testPackagesBinaryAndReadme = testCase "tarball holds an executable hh200 and README.md under one top dir" $
    withWorkspace $ \ws -> do
        (code, _, err) <- runScript ws [] ["v9.9.9"]
        assertEqual ("exit (stderr: " ++ err ++ ")") ExitSuccess code
        tarball <- findTarball ws "v9.9.9"
        listing <- lines <$> readProcess "tar" ["-tzf", tarball] ""
        let top = takeWhile (/= '/') (head (listing ++ [""]))
        assertBool ("top dir named after release: " ++ top) ("hh200-v9.9.9-" `isPrefixOf` top)
        assertBool ("all entries under one top dir: " ++ show listing)
            (all (\e -> (top ++ "/") `isPrefixOf` e || e == top) listing)
        assertBool ("no absolute paths: " ++ show listing) (not (any ("/" `isPrefixOf`) listing))
        let names = map (drop (length top + 1)) listing
        assertBool ("contains hh200: " ++ show names) ("hh200" `elem` names)
        assertBool ("contains README.md: " ++ show names) ("README.md" `elem` names)
        verbose <- readProcess "tar" ["-tvzf", tarball] ""
        let binLine = [ l | l <- lines verbose, ("/hh200" `isSuffixOf` l) ]
        case binLine of
            (l:_) -> assertBool ("hh200 is executable: " ++ l) ('x' `elem` take 10 l)
            []    -> assertFailure "hh200 entry missing from verbose listing"

testChecksumVerifies :: TestTree
testChecksumVerifies = testCase "sha256 file sits next to the tarball and verifies" $ withWorkspace $ \ws -> do
    (code, _, err) <- runScript ws [] ["v9.9.9"]
    assertEqual ("exit (stderr: " ++ err ++ ")") ExitSuccess code
    tarball <- findTarball ws "v9.9.9"
    let sumFile = tarball ++ ".sha256"
    present <- doesFileExist sumFile
    assertBool "checksum file exists" present
    (vcode, vout, verr) <- readCreateProcessWithExitCode
        (proc "sha256sum" ["-c", sumFile]) { cwd = Just (wsWork ws </> "dist") } ""
    assertEqual ("sha256sum -c: " ++ vout ++ verr) ExitSuccess vcode
