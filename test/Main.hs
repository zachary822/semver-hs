{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Data.SemVer
import Test.Hspec
import Text.ParserCombinators.ReadP

main :: IO ()
main = hspec $ do
  describe "parse numeric identifier" $ do
    it "should parse 0" $ do
      readP_to_S pNumeric "0" `shouldMatchList` [(0, "")]
    it "should fail with leading 0" $ do
      readP_to_S pNumeric "01" `shouldMatchList` [(0, "1")]
    it "should parse numbers with 0" $ do
      readP_to_S pNumeric "101" `shouldMatchList` [(101, "")]

  describe "parse alphanumeric identifier" $ do
    it "should not parse 0" $ do
      readP_to_S pAlphaNumeric "0" `shouldMatchList` []
    it "should parse x" $ do
      readP_to_S pAlphaNumeric "x" `shouldMatchList` [("x", "")]
    it "should parse x0" $ do
      readP_to_S pAlphaNumeric "x0" `shouldMatchList` [("x0", "")]
    it "should parse 0x" $ do
      readP_to_S pAlphaNumeric "0x" `shouldMatchList` [("0x", "")]
    it "should parse 0x0" $ do
      readP_to_S pAlphaNumeric "0x0" `shouldMatchList` [("0x0", "")]

  describe "parse pre-release" $ do
    it "should parse alpha" $ do
      readP_to_S pPreRelease "alpha"
        `shouldMatchList` [(PreRelease [PreRelStrSegment "alpha"], "")]
    it "should parse alpha.0" $ do
      readP_to_S pPreRelease "alpha.0"
        `shouldContain` [(PreRelease [PreRelStrSegment "alpha", PreRelIntSegment 0], "")]
    it "should not parse alpha.01" $ do
      readP_to_S pPreRelease "alpha.01"
        `shouldNotContain` [(PreRelease [PreRelStrSegment "alpha", PreRelIntSegment 1], "")]

  describe "parse build" $ do
    it "should parse 001" $ do
      readP_to_S pBuild "001" `shouldMatchList` [(Build ["001"], "")]
    it "should parse exp.sha.5114f85" $ do
      readP_to_S pBuild "exp.sha.5114f85"
        `shouldContain` [(Build ["exp", "sha", "5114f85"], "")]
    it "should parse 21AF26D3----117B344092BD" $ do
      readP_to_S pBuild "21AF26D3----117B344092BD"
        `shouldContain` [(Build ["21AF26D3----117B344092BD"], "")]

  describe "parse version" $ do
    it "should parse 1.0.0" $ do
      readP_to_S pSemVer "1.0.0"
        `shouldMatchList` [(SemVer 1 0 0 Nothing Nothing, "")]
    it "should parse 1.0.0-alpha.0" $ do
      readP_to_S pSemVer "1.0.0-alpha.0"
        `shouldContain` [
                          ( SemVer
                              1
                              0
                              0
                              (Just (PreRelease [PreRelStrSegment "alpha", PreRelIntSegment 0]))
                              Nothing
                          , ""
                          )
                        ]
    it "should parse 1.0.0-alpha+001" $ do
      readP_to_S pSemVer "1.0.0-alpha+001"
        `shouldContain` [
                          ( SemVer
                              1
                              0
                              0
                              (Just (PreRelease [PreRelStrSegment "alpha"]))
                              (Just (Build ["001"]))
                          , ""
                          )
                        ]
    it "read then show should work" $ do
      show (read "1.0.0-alpha+001" :: SemVer) `shouldBe` "1.0.0-alpha+001"
    it "show then read should work" $ do
      read
        ( show
            ( SemVer
                1
                0
                0
                (Just (PreRelease [PreRelStrSegment "alpha"]))
                (Just (Build ["001"]))
            )
        )
        `shouldBe` SemVer
          1
          0
          0
          (Just (PreRelease [PreRelStrSegment "alpha"]))
          (Just (Build ["001"]))
    it "fromString should work on 1.0.0-alpha+001" $ do
      ("1.0.0-alpha+001" :: SemVer)
        `shouldBe` ( SemVer
                      1
                      0
                      0
                      (Just (PreRelease [PreRelStrSegment "alpha"]))
                      (Just (Build ["001"]))
                   )
    it "fromString should throw on 01.0.0-alpha+001" $ do
      evaluate ("01.0.0-alpha+001" :: SemVer)
        `shouldThrow` (\(_ :: SemVerException) -> True)

  describe "compare version" $ do
    it "1.0.0 > 0.9.0" $ do
      (("1.0.0" :: SemVer) `compare` "0.9.0") `shouldBe` GT
    it "1.0.0 == 1.0.0" $ do
      (("1.0.0" :: SemVer) `compare` "1.0.0") `shouldBe` EQ
    it "1.0.0 > 1.0.0-alpha" $ do
      (("1.0.0" :: SemVer) `compare` "1.0.0-alpha") `shouldBe` GT
    it "1.0.0-beta > 1.0.0-alpha" $ do
      (("1.0.0-beta" :: SemVer) `compare` "1.0.0-alpha") `shouldBe` GT
    it "1.0.0-alpha.0 > 1.0.0-alpha" $ do
      (("1.0.0-alpha.0" :: SemVer) `compare` "1.0.0-alpha") `shouldBe` GT
    it "1.0.0-alpha.2 > 1.0.0-alpha.1" $ do
      (("1.0.0-alpha.2" :: SemVer) `compare` "1.0.0-alpha.1") `shouldBe` GT
    it "1.0.0-alpha+001 == 1.0.0-alpha+002" $ do
      (("1.0.0-alpha+001" :: SemVer) `compare` "1.0.0-alpha+002") `shouldBe` EQ
