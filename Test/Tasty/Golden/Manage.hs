{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable, ImplicitParams #-}
-- | Golden test management
module Test.Tasty.Golden.Manage
  (
  -- * Command line helpers
    defaultMain

  -- * The ingredient
  , acceptingTests
  , AcceptTests(..)

  -- * Programmatic API
  , acceptGoldenTests
  )
  where

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Golden.Internal
import Data.Maybe
import Data.Typeable
import Data.Tagged
import Data.Proxy
import Control.Monad.Cont
import Text.Printf
import Options.Applicative
import System.Exit

-- | Like @defaultMain@ from the main tasty package, but also includes the
-- golden test management capabilities.
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithIngredients [acceptingTests, listingTests, consoleTestReporter]

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

acceptingTests :: Ingredient
acceptingTests = TestManager [Option (Proxy :: Proxy AcceptTests)] $
  \opts tree ->
    case lookupOption opts of
      AcceptTests False -> Nothing
      AcceptTests True -> Just $ do
        acceptGoldenTests opts tree
        return True

-- | Get the list of all golden tests in a given test tree
getGoldenTests :: OptionSet -> TestTree -> [(TestName, Golden, GoldenFileFormat)]
getGoldenTests =
  foldTestTree
    trivialFold {
      foldSingle = \opts name t ->
        case cast t of
          Just t -> [(name, t, lookupOption opts)]
          Nothing -> []
    }

-- | «Accept» a golden test, i.e. reset the golden value to the currently
-- produced value
acceptGoldenTest :: (?ff :: GoldenFileFormat) => Golden -> IO ()
acceptGoldenTest (Golden _ getTested _ update) =
  vgRun $ liftIO . update =<< getTested

-- | Accept all golden tests in the test tree
acceptGoldenTests :: OptionSet -> TestTree -> IO ()
acceptGoldenTests opts tests = do
  let gs = getGoldenTests opts tests
  forM_ gs $ \(n,g,ff) -> do
    let ?ff = ff in acceptGoldenTest g
    printf "Accepted %s\n" n
