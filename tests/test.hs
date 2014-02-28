{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden.Advanced
import Data.ByteString.Lazy.Char8 ()

main = defaultMain $ testGroup "normalizeLineEndings"
  [ testCase "No CRs" $
      let s = "abc \n def \n"
      in normalizeLineEndings s @=? s
  , testCase "Simple CRLF string" $
      normalizeLineEndings "abc\r\ndef\r\n" @=? "abc\ndef\n"
  , testCase "CR-only string" $
      let s = "abc \r def \r"
      in normalizeLineEndings s @=? s
  , testCase "Many CRs" $
      let s = "\r\r\r \r\r"
      in normalizeLineEndings s @=? s
  , testCase "Mix of LFs and CRs" $
      normalizeLineEndings "\r\n\r \n\n\r\n\n\r\n\r" @=? "\n\r \n\n\n\n\n\r"
  ]
