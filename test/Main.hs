{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception
import Data.Char
import Data.SemVer
import Test.Hspec
import Test.QuickCheck
import Text.ParserCombinators.ReadP

genIdentifier :: Gen String
genIdentifier = listOf1 (arbitraryASCIIChar `suchThat` isIdentifier)

instance Arbitrary SemVer where
  arbitrary =
    SemVer
      <$> (getNonNegative <$> (arbitrary :: Gen (NonNegative Int)))
      <*> (getNonNegative <$> (arbitrary :: Gen (NonNegative Int)))
      <*> (getNonNegative <$> (arbitrary :: Gen (NonNegative Int)))
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PreRelease where
  arbitrary = PreRelease <$> listOf1 arbitrary

instance Arbitrary PreRelSegment where
  arbitrary =
    frequency
      [
        ( 1
        , PreRelStrSegment
            <$> (genIdentifier `suchThat` any isNonDigit)
        )
      ,
        ( 1
        , PreRelIntSegment . getNonNegative <$> (arbitrary :: Gen (NonNegative Int))
        )
      ]

instance Arbitrary Build where
  arbitrary = Build <$> listOf1 genIdentifier

main :: IO ()
main = hspec $ do
  describe "parse numeric identifier" $ do
    it "should parse number with no leading 0"
      $ property
      $ forAll
        ( liftA2 (:) (chooseEnum ('1', '9')) (listOf arbitrary `suchThat` all isDigit) ::
            Gen String
        )
      $ \n -> readP_to_S pNumeric n `shouldMatchList` [(read n, "")]

    it "should parse 0" $ do
      readP_to_S pNumeric "0" `shouldMatchList` [(0, "")]
    it "should fail with leading 0"
      $ property
      $ forAll
        ( ('0' :) <$> (listOf1 arbitraryASCIIChar `suchThat` all isDigit) ::
            Gen String
        )
      $ \n -> readP_to_S pNumeric n `shouldBe` [(0, drop 1 n)]

    it "should parse number with 0"
      $ property
      $ forAll
        ( liftA2
            (:)
            (chooseEnum ('1', '9'))
            (listOf1 arbitraryASCIIChar `suchThat` all isDigit `suchThat` any (== '0')) ::
            Gen String
        )
      $ \n -> readP_to_S pNumeric n `shouldMatchList` [(read n, "")]

  describe "parse alphanumeric identifier" $ do
    it "should not parse only digit" $
      property $
        forAll (listOf1 arbitrary `suchThat` all isDigit) $ \n ->
          readP_to_S pAlphaNumeric n `shouldMatchList` []

    it "should parse alphanumeric" $
      property $
        forAll (genIdentifier `suchThat` any isNonDigit) $ \n ->
          readP_to_S pAlphaNumeric n `shouldMatchList` [(n, "")]

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
    it "read is inverse to show" $ do
      show (read "1.0.0-alpha+001" :: SemVer) `shouldBe` "1.0.0-alpha+001"
    it "show is inverse to read" $
      property $
        forAll (arbitrary :: Gen SemVer) $
          \ver -> (read . show) ver `shouldBe` ver
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
