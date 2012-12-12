{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Golden.Internal where

import Data.Typeable (Typeable)
import Control.Monad
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error hiding (Error)
import qualified Control.Monad.Error as Er
import Test.Framework.Providers.API hiding (liftIO)
import qualified Test.Framework.Providers.API as TF
import Data.ByteString.Lazy as LB
import Control.Exception
import System.IO
import Data.Maybe

data Error
  = EIO IOException
  | ENotEqual String
  | EOther String

instance Er.Error Error where
  strMsg = EOther

instance Show Error where
  show (EIO e) = show e
  show (ENotEqual s) = s
  show (EOther s) = s

data Golden =
  forall a .
    Golden
      (ValueGetter a)
      (ValueGetter a)
      (a -> a -> IO (Maybe String))
      (a -> IO ())
  deriving Typeable

-- | An action that yields a value (either golden or tested).
-- 'Either' is for possible errors (file not found, parse error etc.), and CPS
-- allows closing the file handle when using lazy IO to read data.
newtype ValueGetter a = ValueGetter
  { runValueGetter :: Er.ErrorT Error (ContT Result IO) a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadError Error)

instance MonadIO ValueGetter where
  liftIO a = ValueGetter $ liftIO (try a) >>= either (throwError . EIO) return

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: FilePath -> ValueGetter ByteString
vgReadFile path =
  (liftIO . LB.hGetContents =<<) $
  ValueGetter $
  ErrorT $
  ContT $ \k ->
  bracket
    (try $ openBinaryFile path ReadMode)
    (either (const $ return ()) hClose)
    (k . either (Left . EIO) Right)

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

go :: Golden -> IO Result
go (Golden getGolden getTested cmp _) =
  ($ (either (return . TestError . show) (const $ return Pass))) $
  runContT $
  runErrorT $
  runValueGetter $
  do
    new <- getTested
    ref <- getGolden

    eq <- liftIO $ cmp ref new

    case eq of
      Nothing -> return ()
      Just e -> throwError $ ENotEqual e
