{-# LANGUAGE MultiParamTypeClasses, Rank2Types, ExistentialQuantification,
             DeriveDataTypeable #-}
module Test.Golden.Advanced
  ( -- * The main function
    goldenTest,

    -- * ValueGetter monad
    ValueGetter(..),
    vgLiftIO,
    vgError,
    vgReadFile,

    -- * Other useful utilities
    Lit(..),
    showLit
  )
where

import Test.Framework.Providers.API as TF
import Data.Maybe
import Data.ByteString.Lazy as LB
import System.IO
import Control.Exception
import Control.Monad
import Control.Applicative
import Data.Typeable (Typeable)

-- | An action that yields a value (either golden or tested).
-- 'Either' is for possible errors (file not found, parse error etc.), and CPS
-- allows closing the file handle when using lazy IO to read data.
--
-- This is essentially EitherT over Codensity over IO, but that leads to too
-- many dependencies.
newtype ValueGetter e a = ValueGetter
  { runValueGetter :: forall r . (Either e a -> IO r) -> IO r }

instance Monad (ValueGetter e) where
  return x = ValueGetter $ \k -> k $ Right x
  ValueGetter a >>= f = ValueGetter $ \k ->
    a $ \x -> case x of Left e -> k $ Left e; Right y -> runValueGetter (f y) k

instance Functor (ValueGetter e) where fmap = liftM
instance Applicative (ValueGetter e) where (<*>) = ap; pure = return

-- | Lift an 'IO' action to 'ValueGetter'
vgLiftIO :: IO a -> ValueGetter e a
vgLiftIO a = ValueGetter $ \k -> a >>= k . Right

-- | Throw an error in the 'ValueGetter' monad
vgError :: e -> ValueGetter e a
vgError e = ValueGetter $ \k -> k $ Left e

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: (IOException -> e) -> FilePath -> ValueGetter e ByteString
vgReadFile wrapException path =
  (vgLiftIO . LB.hGetContents =<<) $
  ValueGetter $ \k ->
  bracket
    (try $ openBinaryFile path ReadMode)
    (either (const $ return ()) hClose)
    (k . either (Left . wrapException) Right)

-- | A very general testing function.
goldenTest
  :: Show e
  => TestName -- ^ test name
  -> ValueGetter e a -- ^ get the golden correct value
  -> ValueGetter e a -- ^ get the tested value
  -> (a -> a -> IO (Maybe e))
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Nothing'. If they are
    -- different, it should return an error that will be printed to the user.
    -- First argument is the golden value.
    --
    -- The function may use 'IO', for example, to launch an external @diff@
    -- command.
  -> (a -> IO ()) -- ^ update the golden file
  -> Test
goldenTest t golden test cmp upd = Test t $ Golden golden test cmp upd

data Golden = forall a e . Show e => Golden
  (ValueGetter e a) (ValueGetter e a) (a -> a -> IO (Maybe e)) (a -> IO ())
  deriving Typeable

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
    ref <- getGolden
    new <- getTested

    eq <- vgLiftIO $ cmp ref new

    case eq of
      Nothing -> return ()
      Just e -> vgError e

-- | A newtype around 'String' whose 'Show' instance produces the string
-- itself.
newtype Lit = Lit String

instance Show Lit where show (Lit s) = s

-- | @showLit = Lit . show@
showLit :: Show a => a -> Lit
showLit = Lit . show
