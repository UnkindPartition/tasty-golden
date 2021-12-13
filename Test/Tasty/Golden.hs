{- |

== Getting Started
To get started with golden testing and this library, see
<https://ro-che.info/articles/2017-12-04-golden-tests Introduction to golden testing>.

This module provides a simplified interface. If you want more, see
"Test.Tasty.Golden.Advanced".

== Filenames
Filenames are looked up in the usual way, Thus relative
names are relative to the processes current working directory.
It is common to run tests from the package's root directory (via @cabal
test@ or @cabal install --enable-tests@), so if your test files are under
the @tests\/@ subdirectory, your relative file names should start with
@tests\/@ (even if your @test.hs@ is itself under @tests\/@, too).

== Line endings

The best way to avoid headaches with line endings
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

== Linking
The test suite should be compiled with @-threaded@ if you want to avoid
blocking any other threads while 'goldenVsFileDiff' and similar functions
wait for the result of the diff command.

== Windows limitations
When using 'goldenVsFileDiff' or 'goldenVsStringDiff' under Windows the exit
code from the diff program that you specify will not be captured correctly
if that program uses @exec@.

More specifically, you will get the exit code of the /original child/
(which always exits with code 0, since it called @exec@), not the exit
code of the process which carried on with execution after @exec@.
This is different from the behavior prescribed by POSIX but is the best
approximation that can be realised under the restrictions of the
Windows process model.  See 'System.Process' for further details or
<https://github.com/haskell/process/pull/168> for even more.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Golden
  (
    -- * Functions to create a golden test
    goldenVsFile
  , goldenVsString
  , goldenVsFileDiff
  , goldenVsStringDiff
    -- * Options
  , SizeCutoff(..)
  , DeleteOutputFile(..)
    -- * Various utilities
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import System.IO
import System.IO.Temp
import qualified System.Process.Typed as PT
import System.Exit
import System.FilePath
import System.Directory
import Control.Exception
import Control.Monad
import qualified Data.Set as Set
import Foreign.C.Error
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import System.Environment
import Data.Maybe

-- | Compare the output file's contents against the golden file's contents
-- after the given action has created the output file.
goldenVsFile
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFile name ref new act =
  goldenTest2
    name
    (readFileStrict ref)
    (act >> readFileStrict new)
    cmp
    upd
    del
  where
  cmp = simpleCmp $ printf "Files '%s' and '%s' differ" ref new
  upd = createDirectoriesAndWriteFile ref
  del = removeFile new

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
--
-- See the notes at the top of this module regarding linking with
-- @-threaded@ and Windows-specific issues.
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
  goldenTest2
    name
    (throwIfDoesNotExist ref)
    act
    (\_ _ -> runDiff (cmdf ref new) sizeCutoff)
    upd
    del
  where
  upd _ = readFileStrict new >>= createDirectoriesAndWriteFile ref
  del = removeFile new

-- If the golden file doesn't exist, throw an isDoesNotExistError that
-- runGolden will handle by creating the golden file before proceeding.
-- See #32.
throwIfDoesNotExist :: FilePath -> IO ()
throwIfDoesNotExist ref = do
  exists <- doesFileExist ref
  unless exists $ ioError $
    errnoToIOError "goldenVsFileDiff" eNOENT Nothing Nothing

-- | Same as 'goldenVsString', but invokes an external diff command.
--
-- See the notes at the top of this module regarding linking with
-- @-threaded@ and Windows-specific issues.
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
    diff_result :: Maybe String <- runDiff cmd sizeCutoff

    return $ flip fmap diff_result $ \diff ->
      printf "Test output was different from '%s'. Output of %s:\n" ref (show cmd) <> diff

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

-- | Like 'writeFile', but uses binary mode. (Needed only when you work
-- with 'String'.)
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

-- | Like 'LBS.writeFile', but also create parent directories if they are
-- missing.
createDirectoriesAndWriteFile
  :: FilePath
  -> LBS.ByteString
  -> IO ()
createDirectoriesAndWriteFile path bs = do
  -- Look for bazel workspace and if it exists, change the root directory for
  -- file creation.
  -- This is useful when used in bazel run context. Your test is run inside the
  -- bazel runfiles directory, but you want to modify the golden references in
  -- your source tree.
  workspace <- fromMaybe "." <$> lookupEnv "BUILD_WORKING_DIRECTORY"

  let dir = takeDirectory path
  createDirectoryIfMissing
    True -- create parents too
    (workspace </> dir)
  LBS.writeFile (workspace </> path) bs

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

unpackUtf8 :: LBS.ByteString -> String
unpackUtf8 = LT.unpack . LT.decodeUtf8

runDiff
  :: [String] -- ^ the diff command
  -> SizeCutoff
  -> IO (Maybe String)
runDiff cmd sizeCutoff =
  case cmd of
    [] -> throwIO $ ErrorCall "tasty-golden: empty diff command"
    prog : args -> do
      let
        procConf =
          PT.setStdin PT.closed
          . PT.setStderr PT.inherit
          $ PT.proc prog args

      (exitCode, out) <- PT.readProcessStdout procConf
      return $ case exitCode of
        ExitSuccess -> Nothing
        _ -> Just . unpackUtf8 . truncateLargeOutput sizeCutoff $ out
