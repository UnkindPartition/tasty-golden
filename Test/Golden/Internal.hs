{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses #-}
module Test.Golden.Internal where

import Data.Typeable (Typeable)
import Control.Monad
import Control.Applicative
import Test.Framework.Providers.API as TF
import Data.ByteString.Lazy as LB
import Control.Exception
import System.IO
import Data.Maybe

data Error
  = EIO IOException
  | NotEqual String

instance Show Error where
  show (EIO e) = show e
  show (NotEqual s) = s

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
--
-- This is essentially EitherT over Codensity over IO, but that leads to too
-- many dependencies.
newtype ValueGetter a = ValueGetter
  { runValueGetter :: forall r . (Either Error a -> IO r) -> IO r }

instance Monad ValueGetter where
  return x = ValueGetter $ \k -> k $ Right x
  ValueGetter a >>= f = ValueGetter $ \k ->
    a $ \x -> case x of Left e -> k $ Left e; Right y -> runValueGetter (f y) k

instance Functor ValueGetter where fmap = liftM
instance Applicative ValueGetter where (<*>) = ap; pure = return

-- | Lift an 'IO' action to 'ValueGetter' and catch possible 'IOException's
vgLiftIO :: IO a -> ValueGetter a
vgLiftIO a = ValueGetter $ \k -> try a >>= k . either (Left . EIO) Right

-- | Throw an error in the 'ValueGetter' monad
vgError :: Error -> ValueGetter a
vgError e = ValueGetter $ \k -> k $ Left e

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: FilePath -> ValueGetter ByteString
vgReadFile path =
  (vgLiftIO . LB.hGetContents =<<) $
  ValueGetter $ \k ->
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
  runValueGetter $
  do
    new <- getTested
    ref <- getGolden

    eq <- vgLiftIO $ cmp ref new

    case eq of
      Nothing -> return ()
      Just e -> vgError $ NotEqual e
