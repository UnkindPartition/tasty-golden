{-# LANGUAGE MultiParamTypeClasses #-}
{- |
  This package provides support for «golden testing».

  A golden test is an IO action that writes its result to a file.
  To pass the test, this output file should be identical to the corresponding
  «golden» file, which contains the correct result for the test.

-}
module Test.Framework.Providers.Golden
  ( goldenVsFile
  , goldenVsString
  )
  where

import Test.Framework.Providers.API
import Text.Printf
import Data.Maybe
import Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8 as LBC
import System.IO
import Control.Exception

data TestedOutput
  = FileOutput
    { outputFile :: FilePath
    , genOutput :: IO ()
    }
  | StringOutput
    { outputString :: IO ByteString }

data Golden = Golden
  { goldenFile :: FilePath
  , testedOutput :: TestedOutput
  , comparator :: ByteString -> ByteString -> Bool
  }

-- | Compare a given file contents against the golden file contents
goldenVsFile
  :: TestName -- ^ test name
  -> (ByteString -> ByteString -> Bool) -- ^ comparison function (e.g. ('==') or 'eqIgnoringWS')
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> Test -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFile name cmp ref new act = Test name $ Golden ref (FileOutput new act) cmp

-- | Compare a given string against the golden file contents
goldenVsString
  :: TestName -- ^ test name
  -> (ByteString -> ByteString -> Bool) -- ^ comparison function (e.g. '==' or 'eqIgnoreSpaces')
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO ByteString -- ^ action that returns a string
  -> Test -- ^ the test verifies that the returned string is the same as the golden file contents
goldenVsString name cmp ref act = Test name $ Golden ref (StringOutput act) cmp

data Result
    = Timeout
    | Pass
    | FileDiffers FilePath FilePath
    | StringDiffers ByteString FilePath
    | NoNew IOException
    | NoGolden IOException

instance Show Result where
    show Timeout  = "Timed out"
    show Pass     = "OK"
    show (FileDiffers new ref) =
      printf "Files '%s' and '%s' differ" new ref
    show (StringDiffers str ref) =
      printf "Test output was different from '%s'. It was:\n---\n%s\n---" ref $ LBC.unpack str
    show (NoNew ex) =
      printf "Could not read test output: %s" $ show ex
    show (NoGolden ex) =
      printf "Could not read golden file: %s" $ show ex

data TestCaseRunning = TestCaseRunning

instance Show TestCaseRunning where
    show TestCaseRunning = "Running"

instance TestResultlike TestCaseRunning Result where
    testSucceeded Pass = True
    testSucceeded _    = False

instance Testlike TestCaseRunning Result Golden where
    testTypeName _ = "Test Cases"

    runTest topts golden = runImprovingIO $ do
        let timeout = unK $ topt_timeout topts
        mb_result <- maybeTimeoutImprovingIO timeout $
            runGolden golden
        return $ fromMaybe Timeout mb_result

runGolden :: Golden -> ImprovingIO TestCaseRunning f Result
runGolden (Golden ref tst cmp) = do
  yieldImprovement TestCaseRunning
  liftIO $ compareStuff cmp ref tst

compareStuff :: (ByteString -> ByteString -> Bool) -> FilePath -> TestedOutput -> IO Result
compareStuff cmp ref tst =
  withTestedOutput tst $ \strNew differ ->
  withFile NoGolden ref $ \h ->
  do
    strGolden <- LB.hGetContents h
    -- force the result while the handles are open
    evaluate $
      if strGolden `cmp` strNew then Pass else differ ref
  where
  withFile
    :: (IOException -> Result)
    -> FilePath
    -> (Handle -> IO Result)
    -> IO Result
  withFile wrapException path act =
    bracket
      (try $ openBinaryFile path ReadMode)
      (either (const $ return ()) hClose)
      (either (return . wrapException) act)

  withTestedOutput
    :: TestedOutput
    -> (ByteString -> (FilePath -> Result) -> IO Result)
    -> IO Result
  withTestedOutput (FileOutput path act) proceed = do
    act
    withFile NoNew path $ \h -> do
      cts <- LB.hGetContents h
      proceed cts $ FileDiffers path
  withTestedOutput (StringOutput act) proceed = do
    result <- act
    proceed result $ StringDiffers result
