{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Distribution.Version
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

{- TODO
Lenses for `Version`, since it's Tree-like now.
-}
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
import Data.Text (Text,pack,unpack,snoc)
import Text.ParserCombinators.Parsec
import TextShow (showt)

---

-- | A Version.
-- This is a *descriptive* parser, based on a collection of version
-- numbers used in the wild.
--
-- Where groups of letters/numbers, separated by `.`, can be
-- further separated by the symbols "_-+:"
data Version = VLeaf [Text] | VNode [Text] VSep Version deriving (Eq,Show)

data VSep = VColon | VHyphen | VPlus | VUnder deriving (Eq,Show)

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
                     , metaOf   :: [VChunk] } deriving (Show)

-- | Two SemVers are equal if all fields except metadata are equal.
instance Eq SemVer where
  (SemVer ma mi pa pr _) == (SemVer ma' mi' pa' pr' _) =
    (ma,mi,pa,pr) == (ma',mi',pa',pr')

instance Ord SemVer where
  compare (SemVer ma mi pa pr _) (SemVer ma' mi' pa' pr' _) =
    case compare (ma,mi,pa) (ma',mi',pa') of
     LT -> LT
     GT -> GT
     EQ -> case (pr,pr') of
            ([],[]) -> EQ
            ([],_)  -> GT
            (_,[])  -> LT
            _       -> compare pr pr'

-- | A single unit of a Version. May be digits or string of characters.
-- Groups of these are called VChunks, and are the identifiers separated
-- by periods in the source.
data VUnit = Digits Int | Str Text deriving (Eq,Show,Read,Ord)

type VChunk = [VUnit]

-- | Parse a version number, as defined above. 
version :: Text -> Either ParseError Version
version = version' . unpack

-- | Parse a version number, where the input is a legacy String.
version' :: String -> Either ParseError Version
version' = parse versionNumber "Version Number"

versionNumber :: Parser Version
versionNumber = try node <|> leaf

leaf :: Parser Version
leaf = VLeaf <$> tchunks <* eof

node :: Parser Version
node = VNode <$> tchunks <*> sep <*> versionNumber

tchunks :: Parser [Text]
tchunks = (pack <$> many1 (letter <|> digit)) `sepBy` char '.'

sep :: Parser VSep
sep = choice [ VColon  <$ char ':'
             , VHyphen <$ char '-'
             , VPlus   <$ char '+'
             , VUnder  <$ char '_' ]

sepCh :: VSep -> Char
sepCh VColon  = ':'
sepCh VHyphen = '-'
sepCh VPlus   = '+'
sepCh VUnder  = '_'

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
digits = read <$> (string "0" <|> many1 digit)

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

iunit :: Parser VUnit
iunit = Digits . read <$> many1 digit

sunit :: Parser VUnit
sunit = Str . pack <$> many1 letter

-- | Convert a Version back to its textual representation.
prettyVer :: Version -> Text
prettyVer (VLeaf t)     = mconcat $ intersperse "." t
prettyVer (VNode t s v) = snoc t' (sepCh s) <> prettyVer v
  where t' = mconcat $ intersperse "." t

-- | Convert a SemVer back to its textual representation.
prettySemVer :: SemVer -> Text
prettySemVer (SemVer ma mi pa pr me) = mconcat $ ver <> pr' <> me'
  where ver = intersperse "." [ showt ma, showt mi, showt pa ]
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)
        me' = foldable [] ("+" :) $ intersperse "." (chunksAsT me)

chunksAsT :: [VChunk] -> [Text]
chunksAsT = map (mconcat . map f)
  where f (Digits i) = showt i
        f (Str s)    = s

-- | Analogous to `maybe` and `either`. If a given Foldable is empty,
-- a default value is returned. Else, a function is applied to that Foldable.
foldable :: Foldable f => f b -> (f a -> f b) -> f a -> f b
foldable d g f | null f    = d
               | otherwise = g f
