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
    , VChunk(..)
    , SemVer(..)
      -- * Parsers
    , version
    , version'
    , semver
    , semver'
      -- * Analysis
    , isSemVer
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
                       , unitsOf    :: [[VChunk]]
                       , revisionOf :: Maybe Int }
               deriving (Eq,Show,Ord)

-- | A single unit of a Version. May be a digit or a character.
-- Groups of these are separated by periods.
data VChunk = Digits Int | Str String deriving (Eq,Show,Read,Ord)

-- | A Version that conforms to Semantic Versioning.
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
                     , preRelOf :: [[VChunk]]
                     , metaOf   :: [[VChunk]] }
              deriving (Eq,Show,Ord)

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

units :: Parser [[VChunk]]
units = many1 (iunit <|> sunit) `sepBy` oneOf "._+"

iunit :: Parser VChunk
iunit = Digits . read <$> many1 digit

sunit :: Parser VChunk
sunit = Str <$> many1 letter

rev :: Parser Int
rev = char '-' *> pure read <*> many1 digit

semver :: Text -> Either ParseError SemVer
semver = undefined

semver' :: String -> Either ParseError SemVer
semver' = undefined

semanticVersion :: Parser SemVer
semanticVersion = SemVer <$> major <*> minor <*> patch <*> preRel <*> metaData

major :: Parser Int
major = undefined

minor :: Parser Int
minor = undefined

patch :: Parser Int
patch = undefined

preRel :: Parser [[VChunk]]
preRel = undefined

metaData :: Parser [[VChunk]]
metaData = undefined

-- | Does a given Version conform to Semantic Versioning?
isSemVer :: Version -> Bool
isSemVer = undefined

-- | Convert a Version back to its textual representation.
-- TODO: Use `chunksAsT` here.
prettyVer :: Version -> Text
prettyVer v = e <> u <> r
  where e = maybe empty (\ep -> showt ep <> ":") $ epochOf v
        u = mconcat . intersperse "." $ map f (unitsOf v)
        f = mconcat . map g
        g (Digits i) = showt i
        g (Str s)    = pack s
        r = maybe empty (\re -> "-" <> showt re) $ revisionOf v

-- | Convert a SemVer back to its textual representation.
prettySemVer :: SemVer -> Text
prettySemVer (SemVer ma mi pa pr me) = mconcat $ ver ++ [pr,me]
  where ver = intersperse "." [ showt ma, showt mi, showt pa ]
        pr  = undefined--"-" : intersperse "." $ map
        me  = undefined

chunksAsT :: [[VChunk]] -> [Text]
chunksAsT = map (mconcat . map f)
  where f (Digits i) = showt i
        f (Str s)    = pack s


-- Write tests
-- (pretty-print <$> version) should map to itself
-- `isSemVer :: Version -> Bool`
