{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{- |
This module provides a simplified interface. If you want more, see "Test.Tasty.Golden.Advanced".

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

module Test.Tasty.Golden.Simple
  ( goldenVsFile
  , goldenVsProg
  , goldenVsAction

  , GoldenTextLike (..)
  , ProgramResult
  )
  where

import Test.Tasty.Providers
import Test.Tasty.Golden.Advanced
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.Exit
import Control.Applicative
import qualified Data.Text as T
import System.Process.Text as PT

-- trick to avoid an explicit dependency on transformers
import Control.Monad.Error (liftIO)
import Data.Text.Encoding


-- | Compare a given file contents against the golden file contents. Assumes that both text files are utf8 encoded.
goldenVsFile
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFile name ref new act =
  goldenTest1
    name
    (maybe Nothing (Just . decodeUtf8 . BL.toStrict) <$> vgReadFileMaybe ref)
    (liftIO act >> (decodeUtf8 . BL.toStrict <$> vgReadFile new))
    textLikeDiff
    textLikeShow
    (upd)
  where upd = BS.writeFile ref . encodeUtf8

-- | Compares a given file with the output (exit code, stdout, stderr) of a program. Assumes
-- that the program output is utf8 encoded.
goldenVsProg
  :: TestName   -- ^ test name
  -> FilePath   -- ^ path to the golden file
  -> FilePath   -- ^ executable to run.
  -> [String]   -- ^ arguments to pass.
  -> T.Text     -- ^ stdin
  -> TestTree
goldenVsProg name ref cmd args inp =
  goldenVsAction name ref runProg
  where runProg = PT.readProcessWithExitCode cmd args inp

-- | Compare something text-like against the golden file contents.
goldenVsAction
  :: GoldenTextLike a
  => TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO a -- ^ action that returns a text-like value.
  -> TestTree -- ^ the test verifies that the returned textual representation (see 'GoldenTextLike')
              --   is the same as the golden file contents
goldenVsAction name ref act =
  goldenTest1
    name
    (maybe Nothing (Just . decodeUtf8 . BL.toStrict) <$> vgReadFileMaybe ref)
    (liftIO (printGT <$> act))
    textLikeDiff
    textLikeShow
    (upd . BL.fromStrict . encodeUtf8)
  where upd = BL.writeFile ref

textLikeShow :: T.Text -> GShow
textLikeShow = ShowText

textLikeDiff :: T.Text -> T.Text -> GDiff
textLikeDiff x y | x == y    = Equal
textLikeDiff x y | otherwise =  DiffText x y

-- | The result of running a program.
type ProgramResult bs = (ExitCode, bs, bs)


-- | How a value can be converted to a textual representation.
class GoldenTextLike a where
  printGT :: a -> T.Text

instance GoldenTextLike T.Text where
  printGT = id

instance GoldenTextLike (ProgramResult T.Text) where
  printGT = printProgRes id


-- first line is exit code, then out block, then err block
printProgRes :: (bs -> T.Text) ->  ProgramResult bs -> T.Text
printProgRes dec (ex, a, b) = T.unlines (["ret > " `T.append` T.pack (show ex)]
                            ++ addPref "out" (dec a) ++ addPref "err" (dec b))
    where addPref pref t = let body = case T.lines t of
                                    [] -> []
                                    (x:xs) -> ((pref `T.append` " > " `T.append` x):(map (T.append "    > ") xs))
                               nlFix = if "\n" `T.isSuffixOf` t then ["    > "] else [] -- avoid trailing \n to get lost
                            in body ++ nlFix

-- | Assumes that the bytestring is utf8 encoded.
instance GoldenTextLike BS.ByteString where
  printGT = decodeUtf8

-- | Assumes that the bytestring is utf8 encoded.
instance GoldenTextLike BL.ByteString where
  printGT = decodeUtf8 . BL.toStrict


