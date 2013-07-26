{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
-- | Golden test management
module Test.Tasty.Golden.Manage
  (
  -- * Command line helpers
    defaultMain
  , defaultMainWithRunner
  , goldenManagerParser

  -- * Programmatic API
  , acceptGoldenTests
  )
  where

import Test.Tasty hiding (defaultMainWithRunner, defaultMain)
import Test.Tasty.Runners hiding (defaultMainWithRunner)
import Test.Tasty.Options
import Test.Tasty.Golden.Internal
import Data.Maybe
import Data.Typeable
import Control.Monad.Cont
import Text.Printf
import Options.Applicative

-- | Parse possible management commands. Fail (as a parser) if no
-- management commands are given.
goldenManagerParser :: Parser (OptionSet -> TestTree -> IO ())
goldenManagerParser =
  flag'
    acceptGoldenTests
    (  long "accept"
    <> help "Accept current results of golden tests"
    )

-- | Parse the command line arguments and run the tests using the provided
-- runner.
--
-- If any golden test management commands are specified, execute them
-- instead.
--
-- Note: this is a replacement for "Test.Tasty"'s 'defaultMainWithRunner'
-- and has a name conflict with it. You'll need to use @hiding@ or
-- a similar means to resolve this.
defaultMainWithRunner :: Runner -> TestTree -> IO ()
defaultMainWithRunner runner testTree = do
  let
    runTests opts =
      runner opts testTree =<< launchTestTree opts testTree

    optsParser = treeOptionParser testTree

    -- partially apply goldenManagerParser to testTree
    mgmntParser :: Parser (OptionSet -> IO ())
    mgmntParser =
      (\mgr opts -> mgr opts testTree) <$> goldenManagerParser

    parser =
      (mgmntParser <|> pure runTests) <*> optsParser

  join $ execParser $
    info (helper <*> parser)
    ( fullDesc <>
      header "Mmm... tasty test suite (with golden test management capabilities)"
    )

-- | Parse the command line arguments and run the tests using the standard
-- console runner.
--
-- If any golden test management commands are specified, execute them
-- instead.
--
-- Note: this is a replacement for "Test.Tasty"'s 'defaultMain'
-- and has a name conflict with it. You'll need to use @hiding@ or
-- a similar means to resolve this.
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithRunner runUI

-- | Get the list of all golden tests in a given test tree
getGoldenTests :: OptionSet -> TestTree -> [(TestName, Golden)]
getGoldenTests =
  foldTestTree
    (\_ name t -> fmap ((,) name) $ maybeToList $ cast t)
    (const id)

-- | «Accept» a golden test, i.e. reset the golden value to the currently
-- produced value
acceptGoldenTest :: Golden -> IO ()
acceptGoldenTest (Golden _ getTested _ update) =
  vgRun $ liftIO . update =<< getTested

-- | Accept all golden tests in the test tree
acceptGoldenTests :: OptionSet -> TestTree -> IO ()
acceptGoldenTests opts tests = do
  let gs = getGoldenTests opts tests
  forM_ gs $ \(n,g) -> do
    acceptGoldenTest g
    printf "Accepted %s\n" n
