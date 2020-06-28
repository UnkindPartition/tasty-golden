{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Process.Typed
import Data.List (sort)

touch f = writeFile f ""
diff ref new = ["diff", "-u", ref, new]

-- Remove a .golden file that a golden test created due to the .golden file not
-- existing prior to the start of the test.
postCleanup :: String -> (FilePath -> TestTree) -> TestTree
postCleanup fileName f =
  withResource (return ()) (\_ -> removeFile filePath) (\_ -> f filePath)
  where
    filePath = "tests/golden" </> fileName <.> "golden"

main = defaultMain $ testGroup "Tests"
  [ testCase "findByExtension" $
    withSystemTempDirectory "golden-test" $ \basedir -> do

      createDirectory (basedir </> "d1")
      createDirectory (basedir </> "d1" </> "d2")
      touch (basedir </> "f1.c")
      touch (basedir </> "f2.h")
      touch (basedir </> "f2.exe")
      touch (basedir </> "d1" </> "g1.c")
      touch (basedir </> "d1" </> "d2" </> "h1.c")
      touch (basedir </> "d1" </> "d2" </> "h1.exe")
      touch (basedir </> "d1" </> "d2" </> "h1")

      files <- findByExtension [".c", ".h"] basedir
      sort files @?= (sort . map (basedir </>))
        ["d1/d2/h1.c","d1/g1.c","f1.c","f2.h"]
  , testGroup "Missing golden files"
    -- Make sure that each entrypoint to tasty-golden can properly create
    -- golden files if they are not provided. This serves as a regression test
    -- for #32.
    [ postCleanup "goldenVsFile" $ \golden ->
      goldenVsFile
        "goldenVsFile without golden file"
        golden
        "tests/golden/goldenVsFile.actual"
        (touch "tests/golden/goldenVsFile.actual")
    , postCleanup "goldenVsFileDiff" $ \golden ->
      goldenVsFileDiff
        "goldenVsFileDiff without golden file"
        diff
        golden
        "tests/golden/goldenVsFileDiff.actual"
         (touch "tests/golden/goldenVsFileDiff.actual")
    , postCleanup "goldenVsString" $ \golden ->
      goldenVsString
        "goldenVsString without golden file"
        golden
        (return "")
    , postCleanup "goldenVsStringDiff" $ \golden ->
      goldenVsStringDiff
        "goldenVsStringDiff without golden file"
        diff
        golden
        (return "")
    ]
#ifdef BUILD_EXAMPLE
  , withResource
    (do
      tmp0 <- getCanonicalTemporaryDirectory
      tmp <- createTempDirectory tmp0 "golden-test"
      runProcess_ $ shell $ "cp -r example " ++ tmp
      return tmp
    )
    ({-removeDirectoryRecursive-}const $ return ()) $ \tmpIO ->
    testGroup "Example test suite"
    [ goldenVsFileDiff
        "before --accept"
        diff
        "tests/golden/before-accept.golden"
        "tests/golden/before-accept.actual"
        (do
          tmp <- tmpIO
          our <- getCurrentDirectory
          -- The sed invocation is used to get rid of the differences
          -- caused by random file names in goldenVsStringDiff and by the
          -- timings.
          --
          -- NB: cannot use multiline literals because of CPP.
          let cmd = shell ("cd " ++ tmp ++ " && example | " ++
                      "sed -Ee 's/[[:digit:]-]+\\.actual/.actual/g; s/ \\([[:digit:].]+s\\)//' > " ++
                      our</>"tests/golden/before-accept.actual || true")
          runProcess_ cmd
        )
    , after AllFinish "/before --accept/" $ goldenVsFileDiff
        "with --accept"
        diff
        "tests/golden/with-accept.golden"
        "tests/golden/with-accept.actual"
        (do
          tmp <- tmpIO
          our <- getCurrentDirectory
          let cmd = shell ("cd " ++ tmp ++ " && example --accept | sed -Ee 's/ \\([[:digit:].]+s\\)//' > " ++
                          our </>"tests/golden/with-accept.actual")
          runProcess_ cmd
        )
    , after AllFinish "/with --accept/" $ goldenVsFileDiff
        "after --accept"
        diff
        "tests/golden/after-accept.golden"
        "tests/golden/after-accept.actual"
        (do
          tmp <- tmpIO
          our <- getCurrentDirectory
          let cmd = shell ("cd " ++ tmp ++ " && example | sed -Ee 's/ \\([[:digit:].]+s\\)//' > " ++
                          our</>"tests/golden/after-accept.actual")
          runProcess_ cmd
        )
    ]
#endif
  ]
