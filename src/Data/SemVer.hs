{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SemVer where

import Control.Exception
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List
import Data.Ord
import Data.String
import GHC.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Printf

data SemVerException = SemVerParseException deriving (Show)

instance Exception SemVerException

data SemVer = SemVer
  { major :: !Int
  , minor :: !Int
  , patch :: !Int
  , preRelease :: !(Maybe PreRelease)
  , build :: !(Maybe Build)
  }

instance Eq SemVer where
  (SemVer ma1 mi1 p1 pre1 _) == (SemVer ma2 mi2 p2 pre2 _) = ma1 == ma2 && mi1 == mi2 && p1 == p2 && pre1 == pre2

instance Ord SemVer where
  v1@(SemVer ma1 mi1 p1 pre1 _) `compare` v2@(SemVer ma2 mi2 p2 pre2 _)
    | ma1 > ma2 = GT
    | ma1 == ma2 && mi1 > mi2 = GT
    | ma1 == ma2 && mi1 == mi2 && p1 > p2 = GT
    | ma1 == ma2
        && mi1 == mi2
        && p1 == p2
        && Down (Down <$> pre1) > Down (Down <$> pre2) =
        GT
    | v1 == v2 = EQ
    | otherwise = LT

instance Show SemVer where
  show SemVer{..} =
    printf
      "%d.%d.%d%s%s"
      major
      minor
      patch
      (maybe "" (('-' :) . show) preRelease)
      (maybe "" (('+' :) . show) build)

instance Read SemVer where
  readPrec = lift pSemVer

instance IsString SemVer where
  fromString s = case (filter (null . snd)) (readP_to_S pSemVer s) of
    (res, "") : _ -> res
    _ -> throw SemVerParseException

newtype PreRelease = PreRelease [PreRelSegment] deriving (Eq, Ord)

instance Read PreRelease where
  readPrec = lift pPreRelease

instance Show PreRelease where
  show (PreRelease segments) = intercalate "." (map show segments)

data PreRelSegment
  = PreRelStrSegment String
  | PreRelIntSegment Int
  deriving (Eq)

instance Ord PreRelSegment where
  PreRelIntSegment _ `compare` PreRelStrSegment _ = LT
  PreRelStrSegment _ `compare` PreRelIntSegment _ = GT
  PreRelIntSegment s1 `compare` PreRelIntSegment s2 = s1 `compare` s2
  PreRelStrSegment s1 `compare` PreRelStrSegment s2 = s1 `compare` s2

instance Show PreRelSegment where
  show (PreRelStrSegment s) = s
  show (PreRelIntSegment s) = show s

newtype Build = Build [String]
  deriving (Eq)

instance Show Build where
  show (Build segments) = intercalate "." segments

instance Read Build where
  readPrec = lift pBuild

isPositiveDigit :: Char -> Bool
isPositiveDigit c = c > '0' && c <= '9'

isLetter :: Char -> Bool
isLetter c = isAsciiUpper c || isAsciiLower c

isNonDigit :: Char -> Bool
isNonDigit c = isLetter c || c == '-'

isIdentifier :: Char -> Bool
isIdentifier c = isDigit c || isNonDigit c

pNumeric :: ReadP Int
pNumeric =
  (char '0' *> pure 0)
    <++ ( do
            d <- satisfy isPositiveDigit
            ds <- munch isDigit
            return (read (d : ds))
        )

pAlphaNumeric :: ReadP String
pAlphaNumeric =
  ((:) <$> satisfy isNonDigit <*> munch isIdentifier)
    <++ ( do
            xs <- many1 (satisfy isIdentifier)
            y <- satisfy isNonDigit
            zs <- munch isIdentifier
            return (xs <> (y : zs))
        )

pPreReleaseIdentifier :: ReadP PreRelSegment
pPreReleaseIdentifier = (PreRelStrSegment <$> pAlphaNumeric) <++ (PreRelIntSegment <$> pNumeric)

pPreRelease :: ReadP PreRelease
pPreRelease = PreRelease <$> sepBy1 pPreReleaseIdentifier (char '.')

pBuildIdentifier :: ReadP String
pBuildIdentifier = pAlphaNumeric <++ munch isDigit

pBuild :: ReadP Build
pBuild = Build <$> sepBy1 pBuildIdentifier (char '.')

pSemVer :: ReadP SemVer
pSemVer = do
  major <- pNumeric
  _ <- char '.'
  minor <- pNumeric
  _ <- char '.'
  patch <- pNumeric

  preRelease <- option Nothing $ char '-' *> (Just <$> pPreRelease)
  build <- option Nothing $ char '+' *> (Just <$> pBuild)

  return $ SemVer{..}
