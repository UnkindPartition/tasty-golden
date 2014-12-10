{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{- |
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

module Test.Tasty.Golden
  ( goldenVsFile
  , goldenVsProg
  , goldenVsAction
  , writeBinaryFile
  , findByExtension

  , GoldenTextLike (..)
  , ProgramResult
  , runParseGT
  )
  where

import Test.Tasty.Providers
import Test.Tasty.Golden.Advanced
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Attoparsec.Text
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.Set as Set
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import System.Process.Text as PT

-- trick to avoid an explicit dependency on transformers
import Control.Monad.Error (liftIO)
import Data.Text.Encoding


-- | Compare a given file contents against the golden file contents. Assumes that the text file is utf8 encoded.
goldenVsFile
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFile name ref new act =
  goldenTest
    name
    (vgReadFile ref)
    (liftIO act >> (fromJust <$> vgReadFile new))
    textLikeDiff
    textLikeShow
    upd
  where upd = BL.writeFile ref

goldenVsProg
  :: TestName
  -> FilePath   -- ^ path to the golden file
  -> FilePath   -- ^ executable to run.
  -> [String]   -- ^ arguments to pass.
  -> T.Text       -- ^ stdin
  -> TestTree
goldenVsProg name ref cmd args inp =
  goldenVsAction name ref runProg
  where runProg = PT.readProcessWithExitCode cmd args inp

-- | Compare a given string against the golden file contents
goldenVsAction
  :: GoldenTextLike a
  => TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO a -- ^ action that returns a string
  -> TestTree -- ^ the test verifies that the returned string is the same as the golden file contents
goldenVsAction name ref act =
  goldenTest
    name
    (maybe Nothing (Just . runParseGT . decodeUtf8 . BL.toStrict) <$> vgReadFile ref)
    (liftIO act)
    textLikeDiff
    textLikeShow
    (upd . BL.fromStrict . encodeUtf8 . printGT)
  where upd = BL.writeFile ref

textLikeShow :: GoldenTextLike a => a -> GShow
textLikeShow = ShowText . printGT

textLikeDiff :: GoldenTextLike a => a -> a -> GDiff
textLikeDiff x y = DiffText (printGT x) (printGT y)

-- | Like 'writeFile', but uses binary mode
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


type ProgramResult bs = (ExitCode, bs, bs)

runParseGT :: GoldenTextLike a => T.Text -> a
runParseGT t = case r of
    Left msg -> error $ "ParseGT failed: " ++ msg
    Right x -> x
  where r = parseOnly (parseGT <* endOfInput) t

class Eq a => GoldenTextLike a where
  printGT :: a -> T.Text
  parseGT :: Parser a

instance GoldenTextLike T.Text where
  printGT = id
  parseGT = takeText

instance GoldenTextLike (ProgramResult T.Text) where
  printGT = printProgRes id
  parseGT = parseProgRes id


-- first line is exit code, then out block, then err block
printProgRes :: (bs -> T.Text) ->  ProgramResult bs -> T.Text
printProgRes dec (ex, a, b) = T.unlines (["ret > " `T.append` T.pack (show ex)]
                            ++ addPref "out" (dec a) ++ addPref "err" (dec b))
    where addPref pref t = let body = case T.lines t of
                                    [] -> []
                                    (x:xs) -> ((pref `T.append` " > " `T.append` x):(map (T.append "    > ") xs))
                               nlFix = if "\n" `T.isSuffixOf` t then ["    > "] else [] -- avoid trailing \n to get lost
                            in body ++ nlFix

parseProgRes :: (T.Text -> bs) -> Parser (ProgramResult bs)
parseProgRes enc = (\a b c -> (read $ T.unpack a, b, c))
        <$ string "ret > " <*> takeTill (=='\n') <* char '\n' <*> pStdstr "out" <*> pStdstr "err"

    where pStdstr pref = ((\x xs -> enc $ T.intercalate "\n" (x:xs)) <$> pLine pref <*> many (pLine "   "))
                    <|> (enc <$> pure T.empty)
          pLine :: T.Text -> Parser T.Text
          pLine pref = string pref *> string " > " *> takeTill (=='\n') <* char '\n'

-- | Assumes that the bytestring is utf8 encoded.
instance GoldenTextLike BS.ByteString where
  printGT = decodeUtf8
  parseGT = encodeUtf8 <$> takeText

-- | Assumes that the bytestring is utf8 encoded.
instance GoldenTextLike BL.ByteString where
  printGT = decodeUtf8 . BL.toStrict
  parseGT = BL.fromStrict . encodeUtf8 <$> takeText


