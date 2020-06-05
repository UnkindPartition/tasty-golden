{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Golden.Advanced
  ( -- * The main functions
    goldenTest,
    goldenTest2
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
goldenTest t golden test cmp upd = singleTest t $ Golden golden test cmp upd (return ())

-- | A variant of 'goldenTest' that also provides for deleting the output
-- file. The 'Internal.DeleteOuputFile' option controls the circumstances in which
-- the output file is to be deleted.
--
-- @since 2.3.4
goldenTest2
  :: TestName -- ^ Test name
  -> IO a
    -- ^ Get the golden correct value
    --
    -- Note that this action may be followed by the update function call.
    --
    -- Therefore, this action *should avoid* reading the file lazily;
    -- otherwise, the file may remain half-open and the update action will
    -- fail.
  -> IO a
    -- ^ Get the tested value (in this case from the output file)
  -> (a -> a -> IO (Maybe String))
    -- ^ Comparison function.
    --
    -- If two values are the same, it should return 'Nothing'. If they are
    -- different, it should return an error that will be printed to the user.
    -- First argument is the golden value.
    --
    -- The function may use 'IO', for example, to launch an external @diff@
    -- command.
  -> (a -> IO ())
    -- ^ Update the golden file
  -> IO ()
    -- ^ Action to delete the output file
  -> TestTree
goldenTest2 t golden test cmp upd del = singleTest t $ Golden golden test cmp upd del
