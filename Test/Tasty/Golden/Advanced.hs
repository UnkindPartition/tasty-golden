{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Golden.Advanced
  ( -- * The main function
    goldenTest,

    GShow (..),
    GDiff (..),

    -- * ValueGetter monad
    ValueGetter(..),
    vgReadFile
  )
where

import Test.Tasty.Providers
import Test.Tasty.Golden.Internal
import qualified Data.Text as T

-- | A very general testing function.
goldenTest
  :: Eq a
  => TestName -- ^ test name
  -> (forall r . ValueGetter r (Maybe a)) -- ^ get the golden correct value
  -> (forall r . ValueGetter r a) -- ^ get the tested value
  -> (a -> a -> GDiff)
    -- ^ comparison function.
    --
    -- If two values are the same, it should return 'Nothing'. If they are
    -- different, it should return an error that will be printed to the user.
    -- First argument is the golden value.
    --
    -- The function may use 'IO', for example, to launch an external @diff@
    -- command.
  -> (a -> GShow) -- ^ Show the golden/actual value.
  -> (a -> IO ()) -- ^ update the golden file
  -> TestTree
goldenTest t golden test diff shw upd = singleTest t $ Golden golden test diff shw upd
