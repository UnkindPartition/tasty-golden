{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
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
import Data.Typeable
import Data.Tagged
import Data.Proxy
import Data.Maybe
import Control.Monad.Cont
import Control.Monad.State
import Control.Exception
import Control.Concurrent.Async
import Text.Printf
import Options.Applicative

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
      AcceptTests True -> Just $
        acceptGoldenTests opts tree

-- | Get the list of all golden tests in a given test tree
getGoldenTests :: OptionSet -> TestTree -> [(TestName, Golden)]
getGoldenTests =
  foldTestTree
    trivialFold { foldSingle = \_ name t -> fmap ((,) name) $ maybeToList $ cast t }

-- | «Accept» a golden test, i.e. reset the golden value to the currently
-- produced value
acceptGoldenTest :: Golden -> IO ()
acceptGoldenTest (Golden _ getTested _ update) =
  vgRun $ liftIO . update =<< getTested

-- | Accept all golden tests in the test tree
acceptGoldenTests :: OptionSet -> TestTree -> IO Bool
acceptGoldenTests opts tests = do
  let gs = getGoldenTests opts tests
  numExns <- flip execStateT (0 :: Int) $ forM_ gs $ \(n,g) -> do
    mbExn <- liftIO $ withAsync (acceptGoldenTest g) waitCatch
    case mbExn of
      Right {} -> liftIO $ printf "Accepted %s\n" n
      Left e -> do
        liftIO $ printf "Error when trying to accept %s: %s\n" n (show (e :: SomeException))
        ne <- get
        put $! ne+1

  -- warn when there were problems
  when (numExns > 0) $
    printf "NOTE: %d tests threw exceptions!\n" numExns

  -- is everything ok?
  return (numExns == 0)
