{-# LANGUAGE MultiParamTypeClasses #-}
module Test.Framework.Providers.Golden
  ( golden )
  where

import Test.Framework.Providers.API
import Text.Printf
import Data.Maybe
import Data.ByteString.Lazy as LB
import System.IO
import Control.Exception

data Golden = Golden
  { goldenRef, goldenNew :: FilePath
  , goldenAction :: IO ()
  }

-- | Create a test-framework test based on a «golden» file
golden
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> Test -- ^ test verifies that the output file is the same as the golden file
golden name ref new act = Test name $ Golden ref new act

data Result
    = Timeout
    | Pass
    | Differ FilePath FilePath
    | NoNew IOException
    | NoGolden IOException

instance Show Result where
    show Timeout  = "Timed out"
    show Pass     = "OK"
    show (Differ ref new) = printf "Files '%s' and '%s' differ" new ref
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
runGolden (Golden ref new act) = do
  yieldImprovement TestCaseRunning
  liftIO $ do
    act
    compareFiles ref new

compareFiles :: FilePath -> FilePath -> IO Result
compareFiles ref new =
  withFile NoGolden ref $ \h1 ->
  withFile NoNew    new $ \h2 ->
  do
    [cts1, cts2] <- mapM LB.hGetContents [h1, h2]
    -- force the result while the handles are open
    evaluate $ if cts1 == cts2 then Pass else Differ ref new
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
