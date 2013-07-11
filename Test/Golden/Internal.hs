{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Golden.Internal where

import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad.Cont
import Test.Framework.Providers.API hiding (liftIO)
import qualified Test.Framework.Providers.API as TF
import Data.ByteString.Lazy as LB
import Control.Exception
import System.IO
import Data.Maybe

-- | See 'goldenTest' for explanation of the fields
data Golden =
  forall a .
    Golden
      (forall r . ValueGetter r a)
      (forall r . ValueGetter r a)
      (a -> a -> IO (Maybe String))
      (a -> IO ())
  deriving Typeable

-- | An action that yields a value (either golden or tested).
--
-- CPS allows closing the file handle when using lazy IO to read data.
newtype ValueGetter r a = ValueGetter
  { runValueGetter :: ContT r IO a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: FilePath -> ValueGetter r ByteString
vgReadFile path =
  (liftIO . LB.hGetContents =<<) $
  ValueGetter $
  ContT $ \k ->
  bracket
    (openBinaryFile path ReadMode)
    hClose
    k

-- | Ensures that the result is fully evaluated (so that lazy file handles
-- can be closed), and catches synchronous exceptions.
vgRun :: ValueGetter r r -> IO (Either SomeException r)
vgRun (ValueGetter a) = handleSyncExceptions $ runContT a evaluate

data Result
  = Timeout
  | Pass
  | TestError String

instance Show Result where
  show Timeout  = "Timed out"
  show Pass     = "OK"
  show (TestError s) = s

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
runGolden g = do
  yieldImprovement TestCaseRunning
  TF.liftIO $ go g

handleSyncExceptions :: IO a -> IO (Either SomeException a)
handleSyncExceptions a =
  catch (Right <$> a) $ \e ->
    case fromException e of
      Just async -> throwIO (async :: AsyncException)
      Nothing -> return $ Left e

go :: Golden -> IO Result
go (Golden getGolden getTested cmp _) = do
  result <- vgRun $ do
    new <- getTested
    ref <- getGolden
    liftIO $ cmp ref new

  return $
    case result of
      Left e -> TestError $ show e
      Right (Just reason) -> TestError reason
      Right Nothing -> Pass
