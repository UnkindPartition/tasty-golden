{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Golden
import Test.QuickCheck
import System.IO.Temp
import System.FilePath
import System.Directory
import Data.List (sort)
import qualified Data.Text as T
import System.Exit
import Control.Applicative
import Data.Attoparsec.Text

touch f = writeFile f ""

main = defaultMain tests

tests = testGroup "tests" $ [findByExtensionTest,  textLikeTests]

findByExtensionTest =
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

textLikeTests = testGroup "GoldenTextLike"
  [ testProperty "PrintParse" $ ((\x -> (Right x) === doParse (printGT x)) :: ProgramResult T.Text -> Property)]
  where doParse = parseOnly (parseGT <* endOfInput)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = map T.pack (shrink $ T.unpack xs)

instance Arbitrary ExitCode where
  arbitrary = oneof [return ExitSuccess, genFail]
    where genFail = ExitFailure <$> arbitrary
