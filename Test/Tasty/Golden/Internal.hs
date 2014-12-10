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

-- | See 'goldenTest1' for explanation of the fields
data Golden =
  forall a .
    Golden
        (forall r . ValueGetter r (Maybe a))    -- Get golden value.
        (forall r . ValueGetter r a)            -- Get actual value.
        (a -> a -> GDiff)                       -- Compare/diff.
        (a -> GShow)                            -- How to produce a show.
        (a -> IO ())                            -- Update golden value.
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
vgReadFile path = maybe err id <$> vgReadFileMaybe path
  where err = error $ "File " ++ path ++ " does not exist."

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
-- Will return 'Nothing' if the file does not exist.
vgReadFileMaybe :: FilePath -> ValueGetter r (Maybe ByteString)
vgReadFileMaybe path = do
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

-- | The comparison/diff result.
data GDiff
  = Equal -- ^ Values are equal.
  | DiffText { gActual :: T.Text, gExpected :: T.Text } -- ^ The two values are different, show a diff between the two given texts.
  | ShowDiffed { gDiff :: T.Text }  -- ^ The two values are different, just show the given text to the user.

-- | How to show a value to the user.
data GShow
  = ShowText T.Text     -- ^ Show the given text.

instance IsTest Golden where
  run _ golden _ = runGolden golden
  testOptions = return []

runGolden :: Golden -> IO Result
runGolden (Golden getGolden getActual cmp _ _) = do
  vgRun $ do
    new <- getActual
    ref' <- getGolden
    case ref' of
      Nothing -> return $ testFailed "Missing golden value."
      Just ref -> do
        -- Output could be arbitrarily big, so don't even try to say what wen't wrong.
        case ref `cmp` new of
          Equal -> return $ testPassed ""
          _     -> return $ testFailed "Result did not match expected output. Use interactive mode to see the full output."
