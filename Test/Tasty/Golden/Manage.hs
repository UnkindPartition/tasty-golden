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
import qualified Data.Text as T
import Data.Text.Encoding
import Options.Applicative
import System.Process.ByteString.Lazy as PL
import System.Process
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import System.IO
import System.IO.Temp
import System.FilePath

-- | Like @defaultMain@ from the main tasty package, but also includes the
-- golden test management capabilities.
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithIngredients [interactiveTests, acceptingTests, listingTests, consoleTestReporter]

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

newtype Interactive = Interactive Bool
  deriving (Eq, Ord, Typeable)
instance IsOption Interactive where
  defaultValue = Interactive False
  parseValue = fmap Interactive . safeRead
  optionName = return "interactive"
  optionHelp = return "Run tests in interactive mode."
  optionCLParser =
    fmap Interactive $
    switch
      (  long (untag (optionName :: Tagged Interactive String))
      <> help (untag (optionHelp :: Tagged Interactive String))
      )


acceptingTests :: Ingredient
acceptingTests = TestManager [Option (Proxy :: Proxy AcceptTests)] $
  \opts tree ->
    case lookupOption opts of
      AcceptTests False -> Nothing
      AcceptTests True -> Just $
        acceptGoldenTests opts tree

interactiveTests :: Ingredient
interactiveTests = TestManager [Option (Proxy :: Proxy Interactive)] $
  \opts tree ->
    case lookupOption opts of
      Interactive False -> Nothing
      Interactive True -> Just $
        runTestsInteractive opts tree

-- | Get the list of all golden tests in a given test tree
getGoldenTests :: OptionSet -> TestTree -> [(TestName, Golden)]
getGoldenTests =
  foldTestTree
    trivialFold { foldSingle = \_ name t -> fmap ((,) name) $ maybeToList $ cast t }

-- | «Accept» a golden test, i.e. reset the golden value to the currently
-- produced value
acceptGoldenTest :: Golden -> IO ()
acceptGoldenTest (Golden _ getTested _  _ update) =
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
        _ <- liftIO $ printf "Error when trying to accept %s: %s\n" n (show (e :: SomeException))
        ne <- get
        put $! ne+1

  -- warn when there were problems
  when (numExns > 0) $
    printf "NOTE: %d tests threw exceptions!\n" numExns
  -- is everything ok?
  return (numExns == 0)

-- | Run in interactive mode (only tested on linux)
runTestsInteractive :: OptionSet -> TestTree -> IO Bool
runTestsInteractive opts tests = do
  let gs = getGoldenTests opts tests

  liftIO $ hSetBuffering stdout NoBuffering

  (nFail, nReject) <- flip execStateT (0, 0) $ forM_ gs runTest

  -- warn when there were problems
  when (nFail > 0 || nReject > 0) (do
    _ <- printf "NOTE: %d tests threw exceptions!\n" nFail
    printf "NOTE: %d tests were rejected!\n" nReject
    )

  -- is everything ok?
  return (nFail == 0 && nReject == 0)
  where runTest :: (TestName, Golden) -> StateT (Integer, Integer) IO ()
        runTest (n, (Golden getGolden getActual cmp shw upd)) = do
            -- execute test
            liftIO $ putStrLn "Executing test"

            -- we can't run any update inside the vg monad,
            -- as the golden file might still be open
            (pFail, pReject, act) <- liftIO $ vgRun $ do
              tested <- getActual

              -- read golden
              liftIO $ putStrLn "Getting golden"
              golden <- getGolden
              case golden of
                Nothing -> do
                  _ <- liftIO $ printf "%s: No golden value. Press <enter> to see actual value.\n" n
                  _ <- liftIO getLine
                  _ <- liftIO $ shw tested >>= showValue n
                  liftIO $ tryAccept n upd tested
                Just golden' -> do
                  cmp' <- liftIO $ cmp golden' tested
                  case cmp' of
                    Equal -> do
                      _ <- liftIO $ printf "%s: Golden value matches output.\n" n
                      return (0, 0, return ())
                    diff' -> do
                      _ <- liftIO $ printf "%s: Output does not match golden value. Press <enter> to see diff.\n" n
                      _ <- liftIO getLine
                      _ <- liftIO $ showDiff n diff'
                      liftIO $ tryAccept n upd tested
            -- now execute update
            liftIO act
            modify (\(nFail, nReject) -> (nFail + pFail, nReject + pReject))

        tryAccept :: TestName -> (a -> IO ()) -> a -> IO (Integer, Integer, IO ())
        tryAccept n upd new = do
            _ <- printf "%s: Accept actual value as new golden value? [yn]" n
            ans <- getLine
            case ans of
              "y" -> do
                    return (0, 0, upd new)
              "n" -> return (0, 1, return ())
              _   -> do
                    putStrLn "Invalid answer."
                    tryAccept n upd new

showDiff :: TestName -> GDiff -> IO ()
showDiff n (DiffText tGold tAct) = do
  withSystemTempFile (n <.> "golden") (\fGold hGold -> do
    withSystemTempFile (n <.> "actual") (\fAct hAct -> do
      hSetBinaryMode hGold True
      hSetBinaryMode hAct True
      BS.hPut hGold (encodeUtf8 tGold)
      BS.hPut hAct (encodeUtf8 tAct)
      hClose hGold
      hClose hAct
      callProcess "sh"
        ["-c", "git diff --color=always --no-index --text " ++ fGold ++ " " ++ fAct ++ " | less -r > /dev/tty"]
      )
    )
showDiff n (ShowDiffed t) = showInLess n t
showDiff _ Equal = error "Can't show diff for equal values..."

showValue :: TestName -> GShow -> IO ()
showValue n (ShowText t) = showInLess n t

showInLess :: String -> T.Text -> IO ()
showInLess _ t = do
  -- TODO error handling...
  _ <- PL.readProcessWithExitCode "sh" ["-c", "less > /dev/tty"] inp
  return ()
  where inp = B.fromStrict $ encodeUtf8 t
