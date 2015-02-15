{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Golden.Advanced
  ( -- * The main function
    goldenTest
  )
where

import Test.Tasty.Providers
import Test.Tasty.Golden.Internal

-- | A very general testing function.
goldenTest
  :: TestName -- ^ test name
  -> (IO a)
    -- ^ get the golden correct value
    --
    -- Note that this action may be followed by the update function call.
    --
    -- Therefore, this action *should avoid* reading the file lazily;
    -- otherwise, the file may remain half-open and the update action will
    -- fail.
  -> (IO a) -- ^ get the tested value
  -> (a -> a -> IO (Maybe String))
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Nothing'. If they are
    -- different, it should return an error that will be printed to the user.
    -- First argument is the golden value.
    --
    -- The function may use 'IO', for example, to launch an external @diff@
    -- command.
  -> (a -> IO ())
    -- ^ update the golden file
  -> TestTree
goldenTest t golden test cmp upd = singleTest t $ Golden golden test cmp upd
