{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Test.Tasty.Golden.Internal where

import Control.Applicative
import Control.Monad.Cont
import Control.Exception
import Data.Typeable (Typeable)
import Data.ByteString.Lazy as LB
import System.IO
import System.IO.Error
import Test.Tasty.Providers
import qualified Data.Text as T

-- | See 'goldenTest' for explanation of the fields
data Golden =
  forall a .
    Eq a =>
    Golden
        (forall r . ValueGetter r (Maybe a))    -- ^ Get golden value.
        (forall r . ValueGetter r a)            -- ^ Get actual value.
        (a -> a -> GDiff)                       -- ^ How to produce a diff (use Eq instance for equality test).
        (a -> GShow)                            -- ^ How to produce a show.
        (a -> IO ())                            -- ^ Update golden value.
  deriving Typeable

-- | An action that yields a value (either golden or tested).
--
-- CPS allows closing the file handle when using lazy IO to read data.
newtype ValueGetter r a = ValueGetter
  { runValueGetter :: ContT r IO a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)


-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: FilePath -> ValueGetter r (Maybe ByteString)
vgReadFile path = do
  r <- ((maybe (return Nothing) (\h -> liftIO $ (Just <$> LB.hGetContents h))) =<<) $
    ValueGetter $
    ContT $ \k ->
    catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
      (bracket
        (openBinaryFile path ReadMode)
        hClose
        (\h -> k (Just h))
      )
      (const $ k Nothing)
  return $! r

-- | Ensures that the result is fully evaluated (so that lazy file handles
-- can be closed)
vgRun :: ValueGetter r r -> IO r
vgRun (ValueGetter a) = runContT a evaluate

data GDiff
  = DiffText { actual :: T.Text, expected :: T.Text }

data GShow
  = ShowText T.Text

instance IsTest Golden where
  run _ golden _ = runGolden golden
  testOptions = return []

runGolden :: Golden -> IO Result
runGolden (Golden getGolden getActual _ _ _) = do
  vgRun $ do
    new <- getActual
    ref' <- getGolden
    case ref' of
      Nothing -> return $ testFailed "Missing golden value."
      Just ref -> do
        -- Output could be arbitrarily big, so don't even try to say what wen't wrong.
        case ref == new of
          True  -> return $ testPassed ""
          False -> return $ testFailed "Result did not match expected output. Use interactive mode to see the full output."
