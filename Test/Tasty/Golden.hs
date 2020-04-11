{- |
To get started with golden testing and this library, see
<https://ro-che.info/articles/2017-12-04-golden-tests Introduction to golden testing>.

This module provides a simplified interface. If you want more, see
"Test.Tasty.Golden.Advanced".

Note about filenames. They are looked up in the usual way, thus relative
names are relative to the processes current working directory.
It is common to run tests from the package's root directory (via @cabal
test@ or @cabal install --enable-tests@), so if your test files are under
the @tests\/@ subdirectory, your relative file names should start with
@tests\/@ (even if your @test.hs@ is itself under @tests\/@, too).

Note about line endings. The best way to avoid headaches with line endings
(when running tests both on UNIX and Windows) is to treat your golden files
as binary, even when they are actually textual.

This means:

* When writing output files from Haskell code, open them in binary mode
(see 'openBinaryFile', 'withBinaryFile' and 'hSetBinaryMode'). This will
disable automatic @\\n -> \\r\\n@ conversion on Windows. For convenience, this
module exports 'writeBinaryFile' which is just like `writeFile` but opens
the file in binary mode. When using 'ByteString's note that
"Data.ByteString" and "Data.ByteString.Lazy" use binary mode for
@writeFile@, while "Data.ByteString.Char8" and "Data.ByteString.Lazy.Char8"
use text mode.

* Tell your VCS not to do any newline conversion for golden files. For
 git check in a @.gitattributes@ file with the following contents (assuming
 your golden files have @.golden@ extension):

>*.golden	-text

On its side, tasty-golden reads and writes files in binary mode, too.

Why not let Haskell/git do automatic conversion on Windows? Well, for
instance, @tar@ will not do the conversion for you when unpacking a release
tarball, so when you run @cabal install your-package --enable-tests@, the
tests will be broken.

As a last resort, you can strip all @\\r@s from both arguments in your
comparison function when necessary. But most of the time treating the files
as binary does the job.
-}

{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Golden
  ( goldenVsFile
  , goldenVsString
  , goldenVsFileDiff
  , goldenVsStringDiff
  , SizeCutoff(..)
  , writeBinaryFile
  , findByExtension
  , createDirectoriesAndWriteFile
  )
  where

import Test.Tasty
import Test.Tasty.Golden.Advanced
import Test.Tasty.Golden.Internal
import Text.Printf
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import System.IO
import System.IO.Temp
import System.Process
import System.Exit
import System.FilePath
import System.Directory
import Control.Exception
import Control.Monad
import qualified Data.Set as Set

-- | Compare the output file's contents against the golden file's contents
-- after the given action has created the output file.
goldenVsFile
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFile name ref new act =
  goldenTest
    name
    (readFileStrict ref)
    (act >> readFileStrict new)
    cmp
    upd
  where
  cmp = simpleCmp $ printf "Files '%s' and '%s' differ" ref new
  upd = createDirectoriesAndWriteFile ref

-- | Compare a given string against the golden file's contents.
goldenVsString
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO LBS.ByteString -- ^ action that returns a string
  -> TestTree -- ^ the test verifies that the returned string is the same as the golden file contents
goldenVsString name ref act =
  askOption $ \sizeCutoff ->
  goldenTest
    name
    (readFileStrict ref)
    act
    (cmp sizeCutoff)
    upd
  where
  cmp sizeCutoff x y = simpleCmp msg x y
    where
    msg = printf "Test output was different from '%s'. It was:\n" ref <>
      unpackUtf8 (truncateLargeOutput sizeCutoff y)
  upd = createDirectoriesAndWriteFile ref

simpleCmp :: Eq a => String -> a -> a -> IO (Maybe String)
simpleCmp e x y =
  return $ if x == y then Nothing else Just e

-- | Same as 'goldenVsFile', but invokes an external diff command.
goldenVsFileDiff
  :: TestName -- ^ test name
  -> (FilePath -> FilePath -> [String])
    -- ^ function that constructs the command line to invoke the diff
    -- command.
    --
    -- E.g.
    --
    -- >\ref new -> ["diff", "-u", ref, new]
  -> FilePath -- ^ path to the golden file
  -> FilePath -- ^ path to the output file
  -> IO ()    -- ^ action that produces the output file
  -> TestTree
goldenVsFileDiff name cmdf ref new act =
  askOption $ \sizeCutoff ->
  goldenTest
    name
    (return ())
    act
    (cmp sizeCutoff)
    upd
  where
  cmd = cmdf ref new
  cmp sizeCutoff _ _
    | null cmd = error "goldenVsFileDiff: empty command line"
    | otherwise = do
    (_, Just sout, _, pid) <- createProcess (proc (head cmd) (tail cmd)) { std_out = CreatePipe }
    -- strictly read the whole output, so that the process can terminate
    out <- hGetContentsStrict sout

    r <- waitForProcess pid
    return $ case r of
      ExitSuccess -> Nothing
      _ -> Just . unpackUtf8 . truncateLargeOutput sizeCutoff $ out

  upd _ = readFileStrict new >>= createDirectoriesAndWriteFile ref

-- | Same as 'goldenVsString', but invokes an external diff command.
goldenVsStringDiff
  :: TestName -- ^ test name
  -> (FilePath -> FilePath -> [String])
    -- ^ function that constructs the command line to invoke the diff
    -- command.
    --
    -- E.g.
    --
    -- >\ref new -> ["diff", "-u", ref, new]
  -> FilePath -- ^ path to the golden file
  -> IO LBS.ByteString -- ^ action that returns a string
  -> TestTree
goldenVsStringDiff name cmdf ref act =
  askOption $ \sizeCutoff ->
  goldenTest
    name
    (readFileStrict ref)
    (act)
    (cmp sizeCutoff)
    upd
  where
  template = takeBaseName ref <.> "actual"
  cmp sizeCutoff _ actBS = withSystemTempFile template $ \tmpFile tmpHandle -> do

    -- Write act output to temporary ("new") file
    LBS.hPut tmpHandle actBS >> hFlush tmpHandle

    let cmd = cmdf ref tmpFile

    when (null cmd) $ error "goldenVsFileDiff: empty command line"

    (_, Just sout, _, pid) <- createProcess (proc (head cmd) (tail cmd)) { std_out = CreatePipe }
    -- strictly read the whole output, so that the process can terminate
    out <- hGetContentsStrict sout

    r <- waitForProcess pid
    return $ case r of
      ExitSuccess -> Nothing
      _ -> Just (printf "Test output was different from '%s'. Output of %s:\n" ref (show cmd) <> unpackUtf8 (truncateLargeOutput sizeCutoff out))

  upd = createDirectoriesAndWriteFile ref

truncateLargeOutput
  :: SizeCutoff
  -> LBS.ByteString
  -> LBS.ByteString
truncateLargeOutput (SizeCutoff n) str =
  if LBS.length str <= n
    then str
    else
      LBS.take n str <> "<truncated>" <>
      "\nUse --accept or increase --size-cutoff to see full output."

-- | Like 'writeFile', but uses binary mode.
writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f txt = withBinaryFile f WriteMode (\hdl -> hPutStr hdl txt)

-- | Find all files in the given directory and its subdirectories that have
-- the given extensions.
--
-- It is typically used to find all test files and produce a golden test
-- per test file.
--
-- The returned paths use forward slashes to separate path components,
-- even on Windows. Thus if the file name ends up in a golden file, it
-- will not differ when run on another platform.
--
-- The semantics of extensions is the same as in 'takeExtension'. In
-- particular, non-empty extensions should have the form @".ext"@.
--
-- This function may throw any exception that 'getDirectoryContents' may
-- throw.
--
-- It doesn't do anything special to handle symlinks (in particular, it
-- probably won't work on symlink loops).
--
-- Nor is it optimized to work with huge directory trees (you'd probably
-- want to use some form of coroutines for that).
findByExtension
  :: [FilePath] -- ^ extensions
  -> FilePath -- ^ directory
  -> IO [FilePath] -- ^ paths
findByExtension extsList = go where
  exts = Set.fromList extsList
  go dir = do
    allEntries <- getDirectoryContents dir
    let entries = filter (not . (`elem` [".", ".."])) allEntries
    liftM concat $ forM entries $ \e -> do
      let path = dir ++ "/" ++ e
      isDir <- doesDirectoryExist path
      if isDir
        then go path
        else
          return $
            if takeExtension path `Set.member` exts
              then [path]
              else []

-- | Like 'BS.writeFile', but also create parent directories if they are
-- missing.
createDirectoriesAndWriteFile
  :: FilePath
  -> LBS.ByteString
  -> IO ()
createDirectoriesAndWriteFile path bs = do
  let dir = takeDirectory path
  createDirectoryIfMissing
    True -- create parents too
    dir
  LBS.writeFile path bs

-- | Force the evaluation of a lazily-produced bytestring.
--
-- This is important to close the file handles.
--
-- See <https://ro-che.info/articles/2015-05-28-force-list>.
forceLbs :: LBS.ByteString -> ()
forceLbs = LBS.foldr seq ()

readFileStrict :: FilePath -> IO LBS.ByteString
readFileStrict path = do
  s <- LBS.readFile path
  evaluate $ forceLbs s
  return s

hGetContentsStrict :: Handle -> IO LBS.ByteString
hGetContentsStrict h = do
  hSetBinaryMode h True
  s <- LBS.hGetContents h
  evaluate $ forceLbs s
  return s

unpackUtf8 :: LBS.ByteString -> String
unpackUtf8 = LT.unpack . LT.decodeUtf8
