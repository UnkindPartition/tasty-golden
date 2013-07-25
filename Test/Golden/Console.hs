{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
-- | Use functions from this module instead of
-- "Test.Framework.Runners.Console" to enable golden test management
-- options.
module Test.Golden.Console {-(defaultMain, defaultMainWithArgs)-} where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.Options
--import qualified Test.Framework.Runners.Console as TF
import System.Console.GetOpt
import System.IO
import System.Environment
import Test.Golden.Internal
import Data.Monoid
import Data.Maybe
import Data.Typeable
import Data.Either
import Control.Applicative
import Control.Monad.Cont
import Control.Exception
import Text.Printf
import Options.Applicative

getGoldenTests :: OptionSet -> TestTree -> [(TestName, Golden)]
getGoldenTests =
  foldTestTree
    (\_ name t -> fmap ((,) name) $ maybeToList $ cast t)
    (const id)

acceptGoldenTest :: Golden -> IO ()
acceptGoldenTest (Golden _ getTested _ update) =
  vgRun $ liftIO . update =<< getTested


acceptParser :: Parser Bool
acceptParser = switch (long "accept")

{-defaultMainWithRunner :: Runner -> TestTree -> IO ()
defaultMainWithRunner tests runner = do
  args <- getArgs
  defaultMainWithArgs tests args-}

{-combinedOptions :: [OptDescr (Either Cmd SuppliedRunnerOptions)]
combinedOptions =
  map (fmap Right) optionsDescription ++
  map (fmap Left)
    [ Option [] ["accept"]
        (NoArg Accept)
        "update golden tests (all by default; use -t to specify which ones)"
    ]-}

{-defaultMain :: [Test] -> IO ()
defaultMain tests = do
  args <- getArgs
  defaultMainWithArgs tests args

defaultMainWithArgs :: [Test] -> [String] -> IO ()
defaultMainWithArgs tests args = do
  prog_name <- getProgName
  let usage_header = "Usage: " ++ prog_name ++ " [OPTIONS]"

      (opts, _nonopts, errMsgs) = getOpt Permute combinedOptions args

      err = do
        hPutStrLn stderr $
          concat errMsgs ++ usageInfo usage_header combinedOptions

      (ourOpts, tfOptPieces) = partitionEithers opts
      mbTfOpts = mconcat <$> sequence tfOptPieces

  case mbTfOpts of
    Nothing -> err
    Just tfOpts
      | [Accept] <- ourOpts ->
          let pats = fromMaybe [] $ ropt_test_patterns tfOpts
          in doAccept pats tests
      | [] <- ourOpts ->
          TF.defaultMainWithOpts tests tfOpts
    _ -> err-}

doAccept :: OptionSet -> TestTree -> IO ()
doAccept opts tests = do
  let gs = getGoldenTests opts tests
  forM_ gs $ \(n,g) -> do
    acceptGoldenTest g
    printf "Accepted %s\n" n
