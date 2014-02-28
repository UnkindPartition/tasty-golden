{-# LANGUAGE RankNTypes #-}
module Test.Tasty.Golden.Advanced
  ( -- * The main function
    goldenTest,

    -- * ValueGetter monad
    ValueGetter(..),
    vgReadFile,

    -- * Utilities
    normalizeLineEndings
  )
where

import Test.Tasty.Providers
import Test.Tasty.Golden.Internal
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Builder as LB
import Data.Word
import Data.Monoid

-- | A very general testing function.
goldenTest
  :: TestName -- ^ test name
  -> (forall r . ValueGetter r a) -- ^ get the golden correct value
  -> (forall r . ValueGetter r a) -- ^ get the tested value
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
  -> TestTree
goldenTest t golden test cmp upd = singleTest t $ Golden golden test cmp upd

type PrevCR = Bool

-- | Replace @\r\n@ with @\n@.

-- A more efficient way to do this would be to copy contiguous pieces
-- between newlines. But who cares.
normalizeLineEndings :: LB.ByteString -> LB.ByteString
normalizeLineEndings bs =
  LB.toLazyByteString $ LB.foldr go finish bs False
  where
    cr, lf :: Word8
    cr = fromIntegral $ fromEnum '\r'
    lf = fromIntegral $ fromEnum '\n'

    finish :: PrevCR -> LB.Builder
    finish True = LB.word8 cr
    finish False = mempty

    go
      :: Word8
      -> (PrevCR -> LB.Builder)
      ->  PrevCR -> LB.Builder
    go w proceed prevCR =
      let
        thisCR = w == cr
        thisOutput =
          if thisCR
            then mempty
            else LB.word8 w
        output =
          if prevCR then
            if w == lf
              then
                -- drop cr
                LB.word8 lf
              else
                -- don't drop previous cr
                LB.word8 cr <> thisOutput
            else
              thisOutput

      in
        output <> proceed thisCR
