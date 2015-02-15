{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
-- | Previously, accepting tests (by the @--accept@ flag) was done by this
-- module, and there was an ingredient to handle that mode.
--
-- Now it's done as part of a normal test run. When the `--accept` flag is
-- given, it makes golden tests to update the files whenever there is
-- a mismatch. So you no longer need this module. It remains only for
-- backwards compatibility.
module Test.Tasty.Golden.Manage
  (
  -- * Command line helpers
    defaultMain

  -- * The ingredient
  , acceptingTests
  , AcceptTests(..)
  )
  where

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Golden.Internal

-- | This exists only for backwards compatibility. Use
-- 'Test.Tasty.defaultMain' instead.
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithIngredients [acceptingTests, listingTests, consoleTestReporter]

-- | This exists only for backwards compatibility. You don't need to
-- include this anymore.
acceptingTests :: Ingredient
acceptingTests = TestManager [] $ \_ _ -> Nothing
