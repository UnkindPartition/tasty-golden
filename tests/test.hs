{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Process
import Data.List (sort)

touch f = writeFile f ""
diff ref new = ["diff", "-u", ref, new]

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
#ifdef BUILD_EXAMPLE
  , withResource
    (do
      tmp0 <- getCanonicalTemporaryDirectory
      tmp <- createTempDirectory tmp0 "golden-test"
      callProcess "cp" ["-r", "example", tmp]
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
          callCommand ("cd " ++ tmp ++ " && example | " ++
            "sed -Ee 's/[[:digit:]-]+\\.actual/.actual/g; s/ \\([[:digit:].]+s\\)//' > " ++
            our</>"tests/golden/before-accept.actual || true")
        )
    , after AllFinish "/before --accept/" $ goldenVsFileDiff
        "with --accept"
        diff
        "tests/golden/with-accept.golden"
        "tests/golden/with-accept.actual"
        (do
          tmp <- tmpIO
          our <- getCurrentDirectory
          callCommand ("cd " ++ tmp ++ " && example --accept | sed -Ee 's/ \\([[:digit:].]+s\\)//' > " ++ our </>"tests/golden/with-accept.actual")
        )
    , after AllFinish "/with --accept/" $ goldenVsFileDiff
        "after --accept"
        diff
        "tests/golden/after-accept.golden"
        "tests/golden/after-accept.actual"
        (do
          tmp <- tmpIO
          our <- getCurrentDirectory
          callCommand ("cd " ++ tmp ++ " && example | sed -Ee 's/ \\([[:digit:].]+s\\)//' > " ++ our</>"tests/golden/after-accept.actual")
        )
    ]
#endif
  ]
