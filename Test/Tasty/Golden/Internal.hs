{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Tasty.Golden.Internal where

import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad.Cont
import Test.Tasty.Providers
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
-- can be closed)
vgRun :: ValueGetter r r -> IO r
vgRun (ValueGetter a) = runContT a evaluate

instance IsTest Golden where
  run opts golden _ = runGolden golden
  testOptions = return []

runGolden :: Golden -> IO Result
runGolden (Golden getGolden getTested cmp _) = do
  result <- vgRun $ do
    new <- getTested
    ref <- getGolden
    liftIO $ cmp ref new

  return $
    case result of
      Just reason ->
        testFailed reason
      Nothing ->
        testPassed ""
