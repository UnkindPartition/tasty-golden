{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
-- | API for golden test management.
--
-- See "Test.Tasty.Golden.Console" for a simple way to include golden
-- test management in your test suite.
module Test.Golden.Manage
  ( getGoldenTests
  , acceptGoldenTest
  , acceptGoldenTests
  )
  where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Golden.Internal
import Data.Maybe
import Data.Typeable
import Control.Monad.Cont
import Text.Printf

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
