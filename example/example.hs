import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as LBS

all_numbers, non_square_numbers :: [Int]
all_numbers = [1..1000]
non_square_numbers = filter (\x -> (round . sqrt . fromIntegral) x ^ 2 /= x) all_numbers
all_numbers_str = LBS.pack $ unlines $ map show all_numbers
non_square_numbers_str = LBS.pack $ unlines $ map show non_square_numbers
diff ref new = ["diff", ref, new]

main = defaultMain $ localOption (SizeCutoff 140) $ testGroup "Tests"
  [ testGroup (if success then "Successful tests" else "Failing tests") $
    let
      dir = "example/golden" </> if success then "success" else "fail"
      value = if success then all_numbers_str else non_square_numbers_str
    in
    [ let
        golden = dir </> "goldenVsFile.golden"
        actual = dir </> "goldenVsFile.actual"
      in
        goldenVsFile "goldenVsFile" golden actual
          (createDirectoriesAndWriteFile actual value)
    , let
        golden = dir </> "goldenVsFileDiff.golden"
        actual = dir </> "goldenVsFileDiff.actual"
      in
        goldenVsFileDiff "goldenVsFileDiff" diff golden actual
          (createDirectoriesAndWriteFile actual value)
    , let
        golden = dir </> "goldenVsString.golden"
      in
        goldenVsString "goldenVsString" golden (return value)
    , let
        golden = dir </> "goldenVsStringDiff.golden"
      in
        goldenVsStringDiff "goldenVsStringDiff" diff golden (return value)
    ]
  | success <- [True, False]
  ]
