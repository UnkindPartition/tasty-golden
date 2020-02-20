import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.IO.Temp
import System.FilePath
import System.Directory
import Data.List (sort)

touch f = writeFile f ""

main = defaultMain $
  testCase "findByExtension" $
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
