module Test.Golden.Advanced
  ( -- * The main function
    goldenTest,

    -- * ValueGetter monad
    ValueGetter(..),
    vgReadFile,
  )
where

import Test.Framework.Providers.API
import Test.Golden.Internal

-- | A very general testing function.
goldenTest
  :: TestName -- ^ test name
  -> ValueGetter a -- ^ get the golden correct value
  -> ValueGetter a -- ^ get the tested value
  -> (a -> a -> IO (Maybe String))
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Nothing'. If they are
    -- different, it should return an error that will be printed to the user.
    -- First argument is the golden value.
    --
    -- The function may use 'IO', for example, to launch an external @diff@
    -- command.
  -> (a -> IO ()) -- ^ update the golden file
  -> Test
goldenTest t golden test cmp upd = Test t $ Golden golden test cmp upd
