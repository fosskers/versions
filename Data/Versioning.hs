{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Data.Versioning
-- Copyright : (c) Colin Woodbury, 2015
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Data.Versioning
    (
      -- * Types
      Versioning(..)
    , SemVer(..)
    , Version(..)
    , Mess(..)
    , VParser(..)
    , VUnit(..)
    , VChunk
    , VSep(..)
      -- * Parsers
    , semver
    , semver'
    , version
    , version'
    , mess
    , mess'
      -- * Wrapped Parsers
    , parseV
    , semverP
    , versionP
    , messP
      -- * Pretty Printing
    , prettyV
    , prettySemVer
    , prettyVer
    , prettyMess ) where

import Data.List (intersperse)
import Data.Semigroup
import Data.Text (Text,pack,unpack,snoc)
import Text.ParserCombinators.Parsec
import TextShow (showt)

---

-- | A top-level Versioning type. Acts as a wrapper for the more specific
-- types. This allows each subtype to have its own parser, and for said
-- parsers to be composed. This is useful for specifying custom behaviour
-- for when a certain parser fails.
data Versioning = Ideal SemVer | General Version | Complex Mess
                deriving (Eq,Show)

instance Ord Versioning where
  compare (Ideal s)   (Ideal s')   = compare s s'
  compare (General v) (General v') = compare v v'
  compare (Complex m) (Complex m') = compare m m'
  compare (Ideal s)   (General v)  = cmpSV s v
  compare (General v) (Ideal s)    = undefined

cmpSV :: SemVer -> Version -> Ordering
cmpSV (SemVer ma mi pa pr me) (Version cs re) = undefined

-- | An (Ideal) version number that conforms to Semantic Versioning.
-- This is a *prescriptive* parser, meaning it follows the SemVer standard.
-- Legal semvers conform to the following regex:
--
-- @
-- (0|[1-9][0-9]*)[.](0|[1-9][0-9]*)[.](0|[1-9][0-9]*)
-- (-[0-9A-Za-z]+([0-9A-Za-z.]+)?)(+[0-9A-Za-z]+([0-9A-Za-z.]+)?)
-- @
--
-- Extra Rules:
--
-- 1. Pre-release versions have *lower* precedence than normal versions.
--
-- 2. Build metadata does not affect version precedence.
--
-- For more information, see http://semver.org
data SemVer = SemVer { svMajor  :: Int
                     , svMinor  :: Int
                     , svPatch  :: Int
                     , svPreRel :: [VChunk]
                     , svMeta   :: [VChunk] } deriving (Show)

-- | Two SemVers are equal if all fields except metadata are equal.
instance Eq SemVer where
  (SemVer ma mi pa pr _) == (SemVer ma' mi' pa' pr' _) =
    (ma,mi,pa,pr) == (ma',mi',pa',pr')

-- | Build metadata does not affect version precedence.
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

-- | A single unit of a Version. May be digits or a string of characters.
-- Groups of these are called `VChunk`s, and are the identifiers separated
-- by periods in the source.
data VUnit = Digits Int | Str Text deriving (Eq,Show,Read,Ord)

-- | A logical unit of a version number. Can consist of multiple letters
-- and numbers.
type VChunk = [VUnit]

-- | A (General) Version.
-- Not quite as ideal as a `SemVer`, but has some internal consistancy
-- from version to version.
-- Generally conforms to the @x.x.x-x@ pattern.
data Version = Version { vChunks :: [VChunk]
                       , vRel    :: [VChunk] } deriving (Eq,Ord,Show)

{- TODO
instance Ord Version where
  compare (Version cs re) (Version cs' re') = undefined
-}

-- | A (Complex) Mess.
-- This is a *descriptive* parser, based on examples of stupidly
-- crafted version numbers used in the wild.
--
-- Groups of letters/numbers, separated by a period, can be
-- further separated by the symbols @_-+:@
--
-- Unfortunately, @VChunk@s cannot be used here, as some developers have
-- numbers like @1.003.04@ which make parsers quite sad.
--
-- Not guaranteed to have well-defined ordering (Ord) behaviour.
data Mess = VLeaf [Text] | VNode [Text] VSep Mess deriving (Eq,Show)

instance Ord Mess where
  compare (VLeaf l1) (VLeaf l2)   = compare l1 l2
  compare (VNode _ _ _) (VLeaf _) = GT
  compare (VLeaf _) (VNode _ _ _) = LT
  compare (VNode t1 _ v1) (VNode t2 _ v2) | t1 < t2 = LT
                                          | t1 > t2 = GT
                                          | otherwise = compare v1 v2

-- | Developers use a number of symbols to seperate groups of digits/letters
-- in their version numbers.
data VSep = VColon | VHyphen | VPlus | VUnder deriving (Eq,Show)

-- | A wrapper for a parser function. Can be composed via their
-- Semigroup instance, such that a different parser can be tried
-- if a previous one fails.
newtype VParser = VParser { runVP :: Text -> Either ParseError Versioning }

instance Semigroup VParser where
  (VParser f) <> (VParser g) = VParser h
    where h t = either (const (g t)) Right $ f t

-- | Parse a piece of @Text@ into either an (Ideal) SemVer, a (General)
-- Version, or a (Complex) Mess.
parseV :: Text -> Either ParseError Versioning
parseV = runVP $ semverP <> versionP <> messP

-- | A wrapped `SemVer` parser. Can be composed with other parsers.
semverP :: VParser
semverP = VParser $ fmap Ideal . semver

-- | Parse a (Ideal) Semantic Version.
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

-- | A wrapped `Version` parser. Can be composed with other parsers.
versionP :: VParser
versionP = VParser $ fmap General . version

-- | Parse a (General) `Version`, as defined above.
version :: Text -> Either ParseError Version
version = version' . unpack

-- | Parse a `Version`, where the input is a legacy String.
version' :: String -> Either ParseError Version
version' = parse versionNum "Version"

versionNum :: Parser Version
versionNum = Version <$> chunks <*> preRel

-- | A wrapped `Mess` parser. Can be composed with other parsers.
messP :: VParser
messP = VParser $ fmap Complex . mess

-- | Parse a (Complex) `Mess`, as defined above. 
mess :: Text -> Either ParseError Mess
mess = mess' . unpack

-- | Parse a `Mess`, where the input is a legacy String.
mess' :: String -> Either ParseError Mess
mess' = parse messNumber "Mess"

messNumber :: Parser Mess
messNumber = try node <|> leaf

leaf :: Parser Mess
leaf = VLeaf <$> tchunks <* eof

node :: Parser Mess
node = VNode <$> tchunks <*> sep <*> messNumber

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

-- | Convert any parsed Versioning type to its textual representation.
prettyV :: Versioning -> Text
prettyV (Ideal sv)  = prettySemVer sv
prettyV (General v) = prettyVer v
prettyV (Complex m) = prettyMess m

-- | Convert a `SemVer` back to its textual representation.
prettySemVer :: SemVer -> Text
prettySemVer (SemVer ma mi pa pr me) = mconcat $ ver <> pr' <> me'
  where ver = intersperse "." [ showt ma, showt mi, showt pa ]
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)
        me' = foldable [] ("+" :) $ intersperse "." (chunksAsT me)

-- | Convert a `Version` back to its textual representation.
prettyVer :: Version -> Text
prettyVer (Version cs pr) = mconcat $ ver <> pr'
  where ver = intersperse "." $ chunksAsT cs
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)

-- | Convert a `Mess` back to its textual representation.
prettyMess :: Mess -> Text
prettyMess (VLeaf t)     = mconcat $ intersperse "." t
prettyMess (VNode t s v) = snoc t' (sepCh s) <> prettyMess v
  where t' = mconcat $ intersperse "." t

chunksAsT :: [VChunk] -> [Text]
chunksAsT = map (mconcat . map f)
  where f (Digits i) = showt i
        f (Str s)    = s

-- | Analogous to `maybe` and `either`. If a given Foldable is empty,
-- a default value is returned. Else, a function is applied to that Foldable.
foldable :: Foldable f => f b -> (f a -> f b) -> f a -> f b
foldable d g f | null f    = d
               | otherwise = g f
