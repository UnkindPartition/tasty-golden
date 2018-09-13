import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Temp
import System.FilePath
import System.Directory
import Data.List (sort)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

touch f = writeFile f ""

main = defaultMain $ testGroup "tests"
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
      sort files @?= sort
        [ basedir ++ "/d1/d2/h1.c"
        , basedir ++ "/d1/g1.c"
        , basedir ++ "/f1.c"
        , basedir ++ "/f2.h"]
  , goldenVsStringDiffPure C.unpack
      "golden diff"
      ("tests" </> "testDiff.golden")
      (return $ LC.pack "1\n2\n3")
  ]
