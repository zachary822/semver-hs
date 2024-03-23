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
  deriving (Eq)

instance Ord SemVer where
  (SemVer ma1 mi1 p1 pre1 _) `compare` (SemVer ma2 mi2 p2 pre2 _) =
    (ma1, mi1, p1, Down (Down <$> pre1))
      `compare` (ma2, mi2, p2, Down (Down <$> pre2))

instance Read SemVer where
  readPrec = lift pSemVer

instance Show SemVer where
  show SemVer{..} =
    printf
      "%d.%d.%d%s%s"
      major
      minor
      patch
      (maybe "" (('-' :) . show) preRelease)
      (maybe "" (('+' :) . show) build)

instance IsString SemVer where
  fromString s = case parseSemVer s of
    Just res -> res
    Nothing -> throw SemVerParseException

newtype PreRelease = PreRelease {unPreRelease :: [PreRelSegment]}
  deriving (Eq, Ord)

instance Read PreRelease where
  readPrec = lift pPreRelease

instance Show PreRelease where
  show = intercalate "." . map show . unPreRelease

data PreRelSegment
  = PreRelStrSegment String
  | PreRelIntSegment Int
  deriving (Eq)

instance Ord PreRelSegment where
  PreRelIntSegment s1 `compare` PreRelIntSegment s2 = s1 `compare` s2
  PreRelStrSegment s1 `compare` PreRelStrSegment s2 = s1 `compare` s2
  PreRelIntSegment _ `compare` _ = LT
  PreRelStrSegment _ `compare` _ = GT

instance Read PreRelSegment where
  readPrec = lift pPreReleaseIdentifier

instance Show PreRelSegment where
  show (PreRelStrSegment s) = s
  show (PreRelIntSegment s) = show s

newtype Build = Build {unBuild :: [String]}
  deriving (Eq)

instance Read Build where
  readPrec = lift pBuild

instance Show Build where
  show = intercalate "." . unBuild

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
pAlphaNumeric = do
  xs <- munch1 isIdentifier
  if any isNonDigit xs then return xs else pfail

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

parseSemVer :: String -> Maybe SemVer
parseSemVer s = case (filter (null . snd)) (readP_to_S pSemVer s) of
  (res, "") : _ -> Just res
  _ -> Nothing
