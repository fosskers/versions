{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Distribution.Version
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Distribution.Version
    (
      -- * Types
      Version(..)
    , VUnit(..)
    , VChunk
    , SemVer(..)
      -- * Parsers
    , version
    , version'
    , semver
    , semver'
      -- * Pretty Printing
    , prettyVer
    , prettySemVer ) where

import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text,pack,unpack,empty)
import Text.ParserCombinators.Parsec
import TextShow (showt)

---

-- | A Version.
-- This is a *descriptive* parser, based on a collection of version
-- numbers used in the wild.
-- Legal version numbers conform to the following regex:
--
-- @([0-9]+:)?[0-9a-zA-Z._+]+(-[0-9]+)?@
--
-- Where the first group of digits represents the epoch number,
-- the second represents the "traditional" version number
-- (i.e. 1.234) as either a group of digits or characters,
-- and the third is a revision number, as often included in software
-- package releases.
data Version = Version { epochOf    :: Maybe Int
                       , chunksOf   :: [VChunk]
                       , revisionOf :: Maybe Int }
               deriving (Eq,Show,Ord)

-- | A Version that conforms to Semantic Versioning.
-- This is a *prescriptive* parser, meaning it follows the SemVer standard.
-- Legal semvers conform to the following regex:
--
-- @
-- (0|[1-9][0-9]*)[.](0|[1-9][0-9]*)[.](0|[1-9][0-9]*)
-- (-[0-9A-Za-z]+([0-9A-Za-z.]+)?)(+[0-9A-Za-z]+([0-9A-Za-z.]+)?)
-- @
--
-- Extra Rules:
-- 1. Pre-release versions have *lower* precedence than normal versions.
-- 2. Build metadata does not affect version precedence.
--
-- For more information, see http://semver.org
data SemVer = SemVer { majorOf  :: Int
                     , minorOf  :: Int
                     , patchOf  :: Int
                     , preRelOf :: [VChunk]
                     , metaOf   :: [VChunk] }
              deriving (Eq,Show,Ord)

-- | A single unit of a Version. May be digits or string of characters.
-- Groups of these are called VChunks, and are the identifiers separated
-- by periods in the source.
data VUnit = Digits Int | Str String deriving (Eq,Show,Read,Ord)

type VChunk = [VUnit]

-- | Parse a version number, as defined above. 
version :: Text -> Either ParseError Version
version = version' . unpack

-- | Parse a version number, where the input is a legacy String.
version' :: String -> Either ParseError Version
version' = parse versionNumber "Version Number"

versionNumber :: Parser Version
versionNumber =
  Version <$> optionMaybe (try epoch) <*> units <*> optionMaybe rev

epoch :: Parser Int
epoch = read <$> many1 digit <* char ':'

units :: Parser [VChunk]
units = many1 (iunit <|> sunit) `sepBy` oneOf "._+"

iunit :: Parser VUnit
iunit = Digits . read <$> many1 digit

sunit :: Parser VUnit
sunit = Str <$> many1 letter

rev :: Parser Int
rev = char '-' *> pure read <*> many1 digit

-- | Parse a Semantic Version.
semver :: Text -> Either ParseError SemVer
semver = semver' . unpack

-- | Parse a Semantic Version, as a legacy String.
semver' :: String -> Either ParseError SemVer
semver' = parse semanticVersion "Semantic Version"

semanticVersion :: Parser SemVer
semanticVersion = p <* eof
  where p = SemVer <$> major <*> minor <*> patch <*> preRel <*> metaData

-- | Parse a group of digits, which can't be lead by a 0, unless it is 0.
digits :: Parser Int
digits = read <$> (string "0" <|> many1 digit)  -- THIS IS WRONG?

major :: Parser Int
major = digits <* char '.'

minor :: Parser Int
minor = major

patch :: Parser Int
patch = digits

preRel :: Parser [VChunk]
preRel = (char '-' *> chunks) <|> pure []

metaData :: Parser [VChunk]
metaData = (char '+' *> chunks) <|> pure []

chunks :: Parser [VChunk]
chunks = many (iunit <|> sunit) `sepBy` char '.'

-- | Convert a Version back to its textual representation.
prettyVer :: Version -> Text
prettyVer v = e <> u <> r
  where e = maybe empty (\ep -> showt ep <> ":") $ epochOf v
        u = mconcat . intersperse "." . chunksAsT $ chunksOf v
        r = maybe empty (\re -> "-" <> showt re) $ revisionOf v

-- | Convert a SemVer back to its textual representation.
prettySemVer :: SemVer -> Text
prettySemVer (SemVer ma mi pa pr me) = mconcat $ ver ++ pr' ++ me'
  where ver = intersperse "." [ showt ma, showt mi, showt pa ]
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)
        me' = foldable [] ("+" :) $ intersperse "." (chunksAsT me)

chunksAsT :: [VChunk] -> [Text]
chunksAsT = map (mconcat . map f)
  where f (Digits i) = showt i
        f (Str s)    = pack s

-- | Analogous to `maybe` and `either`. If a given Foldable is empty,
-- a default value is returned. Else, a function is applied to that Foldable.
foldable :: Foldable f => f b -> (f a -> f b) -> f a -> f b
foldable d g f | null f    = d
               | otherwise = g f
