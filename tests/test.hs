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

      setCurrentDirectory basedir

      createDirectory ("d1")
      createDirectory ("d1" </> "d2")
      touch ("f1.c")
      touch ("f2.h")
      touch ("f2.exe")
      touch ("d1" </> "g1.c")
      touch ("d1" </> "d2" </> "h1.c")
      touch ("d1" </> "d2" </> "h1.exe")
      touch ("d1" </> "d2" </> "h1")

      files <- findByExtension [".c", ".h"] "."
      sort files @?= sort
        ["./d1/d2/h1.c","./d1/g1.c","./f1.c","./f2.h"]
