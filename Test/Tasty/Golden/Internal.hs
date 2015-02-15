{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Tasty.Golden.Internal where

import Control.Applicative
import Control.Monad.Cont
import Control.DeepSeq
import Control.Exception
import Data.Typeable (Typeable)
import Data.ByteString.Lazy as LB
import System.IO
import Options.Applicative
import Data.Tagged
import Data.Proxy
import Test.Tasty.Providers
import Test.Tasty.Options

-- | See 'goldenTest' for explanation of the fields
data Golden =
  forall a .
    Golden
      (forall r . ValueGetter r a)
      (forall r . ValueGetter r a)
      (a -> a -> IO (Maybe String))
      (a -> IO ())
  deriving Typeable

-- | This option, when set to 'True', specifies that we should run in the
-- «accept tests» mode
newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord, Typeable)
instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeRead
  optionName = return "accept"
  optionHelp = return "Accept current results of golden tests"
  optionCLParser =
    fmap AcceptTests $
    switch
      (  long (untag (optionName :: Tagged AcceptTests String))
      <> help (untag (optionHelp :: Tagged AcceptTests String))
      )

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
-- can be closed)
vgRun :: ValueGetter r r -> IO r
vgRun (ValueGetter a) = runContT a evaluate

instance IsTest Golden where
  run opts golden _ = runGolden golden (lookupOption opts)
  testOptions = return [Option (Proxy :: Proxy AcceptTests)]

runGolden :: Golden -> AcceptTests -> IO Result
runGolden (Golden getGolden getTested cmp update) (AcceptTests accept) = do
  vgRun $ do
    new <- getTested
    ref <- getGolden
    result <- liftIO $ cmp ref new

    case result of
      Just _reason | accept -> do
        -- test failed; accept the new version
        liftIO $ update new
        return $ testPassed "Accepted the new version"

      Just reason -> do
        -- Make sure that the result is fully evaluated and doesn't depend
        -- on yet un-read lazy input
        liftIO $ evaluate . rnf $ reason
        return $ testFailed reason

      Nothing ->
        return $ testPassed ""
