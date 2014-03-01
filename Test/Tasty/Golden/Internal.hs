{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving, ImplicitParams #-}
module Test.Tasty.Golden.Internal where

import Control.Applicative
import Control.Monad.Cont
import Control.DeepSeq
import Control.Exception
import Data.Typeable (Typeable)
import Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Proxy
import System.IO
import Test.Tasty (localOption) -- for docs only
import Test.Tasty.Providers
import Test.Tasty.Options

-- | See 'goldenTest' for explanation of the fields
data Golden =
  forall a .
    Golden
      ((?ff :: GoldenFileFormat) => forall r . ValueGetter r a)
      ((?ff :: GoldenFileFormat) => forall r . ValueGetter r a)
      (a -> a -> IO (Maybe String))
      ((?ff :: GoldenFileFormat) => a -> IO ())
  deriving Typeable

-- | An action that yields a value (either golden or tested).
--
-- CPS allows closing the file handle when using lazy IO to read data.
newtype ValueGetter r a = ValueGetter
  { runValueGetter :: ContT r IO a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

-- | Lazily read a file. The file handle will be closed after the
-- 'ValueGetter' action is run.
vgReadFile :: (?ff :: GoldenFileFormat) => FilePath -> ValueGetter r ByteString
vgReadFile path =
  (liftIO . LB.hGetContents =<<) $
  ValueGetter $
  ContT $ \k ->
  bracket
    (openBinaryFile  path ReadMode)
    hClose
    k

-- | Open a file accordingly to the specified format
openGoldenFile
  :: (?ff :: GoldenFileFormat)
  => FilePath -> IOMode -> IO Handle
openGoldenFile =
  case ?ff of
    GoldenText -> openFile
    GoldenBinary -> openBinaryFile

-- | Ensures that the result is fully evaluated (so that lazy file handles
-- can be closed)
vgRun :: ValueGetter r r -> IO r
vgRun (ValueGetter a) = runContT a evaluate

instance IsTest Golden where
  run opts golden _ = runGolden (lookupOption opts) golden
  testOptions = return
    [Option (Proxy :: Proxy GoldenFileFormat)]

runGolden :: GoldenFileFormat -> Golden -> IO Result
runGolden ff (Golden getGolden getTested cmp _) =
  let ?ff = ff
  in vgRun $ do
    new <- getTested
    ref <- getGolden
    result <- liftIO $ cmp ref new

    case result of
      Just reason -> do
        -- Make sure that the result is fully evaluated and doesn't depend
        -- on yet un-read lazy input
        liftIO $ evaluate $ reason `deepseq` ()
        return $ testFailed reason
      Nothing ->
        return $ testPassed ""

{- | Specifies the file format of golden files.

All @golden@ functions respect this option. Depending on its value, either
'openFile' or 'openBinaryFile' will be used internally, which will affect
line endings conversion on Windows.

No such conversion is done for raw 'ByteString's in functions like
'goldenVsString'. If you are reading such a string from a handle, you may
need to set the appropriate mode for the handle yourself (see 'hSetBinaryMode').

As with any other tasty option, you can set 'GoldenFileFormat' for the
whole tree, any subtree or individual tests using 'localOption'.
-}
data GoldenFileFormat = GoldenText | GoldenBinary
  deriving Typeable

instance IsOption GoldenFileFormat where
  defaultValue = GoldenText
  parseValue "text"   = Just GoldenText
  parseValue "binary" = Just GoldenBinary
  parseValue _        = Nothing
  optionName = return "golden-file-format"
  optionHelp = return "are golden files text or binary?"
