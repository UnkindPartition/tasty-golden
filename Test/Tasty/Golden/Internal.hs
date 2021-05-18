{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveDataTypeable,
    MultiParamTypeClasses, GeneralizedNewtypeDeriving, CPP #-}
module Test.Tasty.Golden.Internal where

import Control.DeepSeq
import Control.Exception
import Control.Monad (when)
import Data.Typeable (Typeable)
import Data.Proxy
import Data.Int
import Data.Char (toLower)
import System.IO.Error (isDoesNotExistError)
import Options.Applicative (metavar)
import Test.Tasty.Providers
import Test.Tasty.Options
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | See 'goldenTest' for explanation of the fields
data Golden =
  forall a .
    Golden
      (IO a)
      (IO a)
      (a -> a -> IO (Maybe String))
      (a -> IO ())
      (IO ())
  deriving Typeable

-- | This option, when set to 'True', specifies that we should run in the
-- «accept tests» mode
newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord, Typeable)
instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeReadBool
  optionName = return "accept"
  optionHelp = return "Accept current results of golden tests"
  optionCLParser = flagCLParser Nothing (AcceptTests True)

-- | This option, when set to 'True', specifies to error when a file does
-- not exist, instead of creating a new file.
newtype NoCreateFile = NoCreateFile Bool
  deriving (Eq, Ord, Typeable)
instance IsOption NoCreateFile where
  defaultValue = NoCreateFile False
  parseValue = fmap NoCreateFile . safeReadBool
  optionName = return "no-create"
  optionHelp = return "Error when golden file does not exist"
  optionCLParser = flagCLParser Nothing (NoCreateFile True)

-- | The size, in bytes, such that the (incorrect) test output is not
-- displayed when it exceeds this size. Numeric underscores are accepted
-- for readability.
--
-- The default value is 1000 (i.e. 1Kb).
--
-- @since 2.3.3
newtype SizeCutoff = SizeCutoff { getSizeCutoff :: Int64 }
  deriving (Eq, Ord, Typeable, Num, Real, Enum, Integral)
instance IsOption SizeCutoff where
  defaultValue = 1000
  showDefaultValue = Just . show . getSizeCutoff
  parseValue = fmap SizeCutoff . safeRead . filter (/= '_')
  optionName = return "size-cutoff"
  optionHelp = return "hide golden test output if it's larger than n bytes"
  optionCLParser = mkOptionCLParser $ metavar "n"

-- | When / whether to delete the test output file,
-- when there is a golden file
--
-- @since 2.3.4
data DeleteOutputFile
  = Never  -- ^ Never delete the output file (default)
  | OnPass -- ^ Delete the output file if the test passes
  | Always -- ^ Always delete the output file. (May not be commonly used,
           --   but provided for completeness.)
  deriving (Eq, Ord, Typeable, Show)

-- | This option controls when / whether the test output file is deleted
-- For example, it may be convenient to delete the output file when a test
-- passes, since it will be the same as the golden file.
--
-- It does nothing if
--
-- * running the test or accessing an existing golden value threw an exception.
--
-- * there is no golden file for the test
instance IsOption DeleteOutputFile where
  defaultValue = Never
  parseValue = parseDeleteOutputFile
  optionName = return "delete-output"
  optionHelp = return "If there is a golden file, when to delete output files"
  showDefaultValue =  Just . displayDeleteOutputFile
  optionCLParser = mkOptionCLParser $ metavar "never|onpass|always"

parseDeleteOutputFile :: String -> Maybe DeleteOutputFile
parseDeleteOutputFile s =
  case map toLower s of
    "never"  -> Just Never
    "onpass" -> Just OnPass
    "always" -> Just Always
    _        -> Nothing

displayDeleteOutputFile :: DeleteOutputFile -> String
displayDeleteOutputFile dof = map toLower (show dof)

instance IsTest Golden where
  run opts golden _ = runGolden golden opts
  testOptions =
    return
      [ Option (Proxy :: Proxy AcceptTests)
      , Option (Proxy :: Proxy NoCreateFile)
      , Option (Proxy :: Proxy SizeCutoff)
      , Option (Proxy :: Proxy DeleteOutputFile)
      ]

runGolden :: Golden -> OptionSet -> IO Result
runGolden (Golden getGolden getTested cmp update delete) opts = do

    mbNew <- try getTested

    case mbNew of
      Left e -> do
        return $ testFailed $ show (e :: SomeException)
      Right new -> do

        mbRef <- try getGolden

        case mbRef of
          Left e
            | Just e' <- fromException e, isDoesNotExistError e' ->
              if noCreate
                then
                  -- Don't ever delete the output file in this case, as there is
                  -- no duplicate golden file
                  return $ testFailed "Golden file does not exist; --no-create flag specified"
                else do
                  update new
                  when (delOut `elem` [Always, OnPass]) delete
                  return $ testPassed "Golden file did not exist; created"

            | Just (_ :: AsyncException) <- fromException e -> throwIO e
            | Just (_ :: IOError) <- fromException e -> throwIO e


            | otherwise -> do
                -- Other types of exceptions may be due to failing to decode the
                -- golden file. In that case, it makes sense to replace a broken
                -- golden file with the current version.
                update new
                when (delOut `elem` [Always, OnPass]) delete
                return $ testPassed $ "Accepted the new version. Was failing with exception:\n" ++ show e

          Right ref -> do

            result <- cmp ref new

            case result of
              Just _reason | accept -> do
                -- test failed; accept the new version
                update new
                when (delOut `elem` [Always, OnPass]) delete
                return $ testPassed "Accepted the new version"

              Just reason -> do
                -- Make sure that the result is fully evaluated and doesn't depend
                -- on yet un-read lazy input
                evaluate . rnf $ reason
                when (delOut == Always) delete
                return $ testFailed reason

              Nothing -> do
                when (delOut `elem` [Always, OnPass]) delete
                return $ testPassed ""
  where
    AcceptTests accept = lookupOption opts
    NoCreateFile noCreate = lookupOption opts
    delOut = lookupOption opts
