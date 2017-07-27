{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- |
-- Module    : Data.Versions
-- Copyright : (c) Colin Woodbury, 2015 - 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- A library for parsing and comparing software version numbers.
--
-- We like to give version numbers to our software in a myriad of different
-- ways. Some ways follow strict guidelines for incrementing and comparison.
-- Some follow conventional wisdom and are generally self-consistent.
-- Some are just plain asinine. This library provides a means of parsing
-- and comparing /any/ style of versioning, be it a nice Semantic Version
-- like this:
--
-- > 1.2.3-r1+git123
--
-- ...or a monstrosity like this:
--
-- > 2:10.2+0.0093r3+1-1
--
-- Please switch to <http://semver.org Semantic Versioning> if you
-- aren't currently using it. It provides consistency in version
-- incrementing and has the best constraints on comparisons.
--
-- == Using the Parsers
-- In general, `parseV` is the function you want. It attempts to parse
-- a given @Text@ using the three individual parsers, `semver`, `version`
-- and `mess`. If one fails, it tries the next. If you know you only want
-- to parse one specific version type, use that parser directly
-- (e.g. `semver`).

module Data.Versions
    (
      -- * Types
      Versioning(..)
    , SemVer(..)
    , Version(..)
    , Mess(..)
    , VUnit(..)
    , VChunk
    , VSep(..)
    , VParser(..)
    , ParsingError
      -- * Parsers
    , semver
    , version
    , mess
      -- ** Wrapped Parsers
    , parseV
    , semverP
    , versionP
    , messP
      -- ** Megaparsec Parsers
    , semver'
    , version'
    , mess'
      -- * Pretty Printing
    , prettyV
    , prettySemVer
    , prettyVer
    , prettyMess
    , parseErrorPretty
      -- * Lenses
      -- **  Traversing Text
    , _Versioning
    , _SemVer
    , _Version
      -- ** Versioning Traversals
    , _Ideal
    , _General
    , _Complex
      -- ** (Ideal) SemVer Lenses
    , svMajor
    , svMinor
    , svPatch
    , svPreRel
    , svMeta
      -- ** (General) Version Lenses
    , vEpoch
    , vChunks
    , vRel
      -- ** Misc. Lenses / Traversals
    , _Digits
    , _Str ) where

import Control.DeepSeq
import Data.Hashable
import Data.List (intersperse)
import Data.Monoid
import Data.Text (Text,pack,unpack,snoc)
import Data.Void
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char

---

-- | A top-level Versioning type. Acts as a wrapper for the more specific
-- types. This allows each subtype to have its own parser, and for said
-- parsers to be composed. This is useful for specifying custom behaviour
-- for when a certain parser fails.
data Versioning = Ideal SemVer | General Version | Complex Mess
  deriving (Eq,Show,Generic,NFData,Hashable)

-- | Comparison of @Ideal@s is always well defined.
--
-- If comparison of @General@s is well-defined, then comparison
-- of @Ideal@ and @General@ is well-defined, as there exists a perfect
-- mapping from @Ideal@ to @General@.
--
-- If comparison of @Complex@es is well-defined, then comparison of @General@
-- and @Complex@ is well defined for the same reason.
-- This implies comparison of @Ideal@ and @Complex@ is also well-defined.
instance Ord Versioning where
  compare (Ideal s)     (Ideal s')    = compare s s'
  compare (General v)   (General v')  = compare v v'
  compare (Complex m)   (Complex m')  = compare m m'
  compare (Ideal s)     (General v)   = compare (vFromS s) v
  compare (General v)   (Ideal s)     = opposite $ compare (vFromS s) v
  compare (General v)   (Complex m)   = compare (mFromV v) m
  compare (Complex m)   (General v)   = opposite $ compare (mFromV v) m
  compare (Ideal s)     m@(Complex _) = compare (General $ vFromS s) m
  compare m@(Complex _) (Ideal s)     = compare m (General $ vFromS s)

-- | Convert a `SemVer` to a `Version`.
vFromS :: SemVer -> Version
vFromS (SemVer m i p r _) = Version Nothing [[Digits m], [Digits i], [Digits p]] r

-- | Convert a `Version` to a `Mess`.
mFromV :: Version -> Mess
mFromV (Version e v r) = maybe affix (\a -> VNode [showt a] VColon affix) e
  where affix = VNode (chunksAsT v) VHyphen $ VLeaf (chunksAsT r)

-- | Traverse some Text for its inner versioning.
--
-- > _Versioning :: Traversal' Text Versioning
--
-- > ("1.2.3" & _Versioning . _Ideal . svPatch %~ (+ 1)) == "1.2.4"
_Versioning :: Applicative f => (Versioning -> f Versioning) -> Text -> f Text
_Versioning f t = either (const (pure t)) (fmap prettyV . f) $ parseV t
{-# INLINE _Versioning #-}

-- | Traverse some Text for its inner SemVer.
--
-- > _SemVer :: Traversal' Text SemVer
_SemVer :: Applicative f => (SemVer -> f SemVer) -> Text -> f Text
_SemVer f t = either (const (pure t)) (fmap prettySemVer . f) $ semver t
{-# INLINE _SemVer #-}

-- | Traverse some Text for its inner Version.
--
-- > _Version :: Traversal' Text Version
_Version :: Applicative f => (Version -> f Version) -> Text -> f Text
_Version f t = either (const (pure t)) (fmap prettyVer . f) $ version t
{-# INLINE _Version #-}

-- | > _Ideal :: Traversal' Versioning SemVer
_Ideal :: Applicative f => (SemVer -> f SemVer) -> Versioning -> f Versioning
_Ideal f (Ideal s) = Ideal <$> f s
_Ideal _ v = pure v
{-# INLINE _Ideal #-}

-- | > _General :: Traversal' Versioning Version
_General :: Applicative f => (Version -> f Version) -> Versioning -> f Versioning
_General f (General v) = General <$> f v
_General _ v = pure v
{-# INLINE _General #-}

-- | > _Complex :: Traversal' Versioning Mess
_Complex :: Applicative f => (Mess -> f Mess) -> Versioning -> f Versioning
_Complex f (Complex m) = Complex <$> f m
_Complex _ v = pure v
{-# INLINE _Complex #-}

-- | An (Ideal) version number that conforms to Semantic Versioning.
-- This is a /prescriptive/ parser, meaning it follows the SemVer standard.
--
-- Legal semvers are of the form: MAJOR.MINOR.PATCH-PREREL+META
--
-- Example: 1.2.3-r1+commithash
--
-- Extra Rules:
--
-- 1. Pre-release versions have /lower/ precedence than normal versions.
--
-- 2. Build metadata does not affect version precedence.
--
-- For more information, see http://semver.org
data SemVer = SemVer { _svMajor  :: Int
                     , _svMinor  :: Int
                     , _svPatch  :: Int
                     , _svPreRel :: [VChunk]
                     , _svMeta   :: [VChunk] } deriving (Show,Generic,NFData,Hashable)

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

instance Monoid SemVer where
  mempty = SemVer 0 0 0 [] []
  SemVer mj mn pa p m `mappend` SemVer mj' mn' pa' p' m' =
    SemVer (mj + mj') (mn + mn') (pa + pa') (p ++ p') (m ++ m')

-- | > svMajor :: Lens' SemVer Int
svMajor :: Functor f => (Int -> f Int) -> SemVer -> f SemVer
svMajor f sv = fmap (\ma -> sv { _svMajor = ma }) (f $ _svMajor sv)
{-# INLINE svMajor #-}

-- | > svMinor :: Lens' SemVer Int
svMinor :: Functor f => (Int -> f Int) -> SemVer -> f SemVer
svMinor f sv = fmap (\mi -> sv { _svMinor = mi }) (f $ _svMinor sv)
{-# INLINE svMinor #-}

-- | > svPatch :: Lens' SemVer Int
svPatch :: Functor f => (Int -> f Int) -> SemVer -> f SemVer
svPatch f sv = fmap (\pa -> sv { _svPatch = pa }) (f $ _svPatch sv)
{-# INLINE svPatch #-}

-- | > svPreRel :: Lens' SemVer Int
svPreRel :: Functor f => ([VChunk] -> f [VChunk]) -> SemVer -> f SemVer
svPreRel f sv = fmap (\pa -> sv { _svPreRel = pa }) (f $ _svPreRel sv)
{-# INLINE svPreRel #-}

-- | > svMeta :: Lens' SemVer Int
svMeta :: Functor f => ([VChunk] -> f [VChunk]) -> SemVer -> f SemVer
svMeta f sv = fmap (\pa -> sv { _svMeta = pa }) (f $ _svMeta sv)
{-# INLINE svMeta #-}

-- | A single unit of a Version. May be digits or a string of characters.
-- Groups of these are called `VChunk`s, and are the identifiers separated
-- by periods in the source.
data VUnit = Digits Int | Str Text deriving (Eq,Show,Read,Ord,Generic,NFData,Hashable)

-- | > _Digits :: Traversal' VUnit Int
_Digits :: Applicative f => (Int -> f Int) -> VUnit -> f VUnit
_Digits f (Digits i) = Digits <$> f i
_Digits _ v = pure v
{-# INLINE _Digits #-}

-- | > _Str :: Traversal' VUnit Text
_Str :: Applicative f => (Text -> f Text) -> VUnit -> f VUnit
_Str f (Str t) = Str <$> f t
_Str _ v = pure v
{-# INLINE _Str #-}

-- | A logical unit of a version number. Can consist of multiple letters
-- and numbers.
type VChunk = [VUnit]

-- | A (General) Version.
-- Not quite as ideal as a `SemVer`, but has some internal consistancy
-- from version to version.
-- Generally conforms to the @x.x.x-x@ pattern, and may optionally have an /epoch/.
-- These are prefixes marked by a colon, like in @1:2.3.4@.
--
-- Examples of @Version@ that are not @SemVer@: 0.25-2, 8.u51-1, 20150826-1, 1:2.3.4
data Version = Version { _vEpoch  :: Maybe Int
                       , _vChunks :: [VChunk]
                       , _vRel    :: [VChunk] } deriving (Eq,Show,Generic,NFData,Hashable)

-- | Set a `Version`'s epoch to `Nothing`.
wipe :: Version -> Version
wipe v = v { _vEpoch = Nothing }

-- | Customized.
instance Ord Version where
  -- | The obvious base case.
  compare (Version _ [] []) (Version _ [] []) = EQ

  -- | For the purposes of Versions with epochs, `Nothing` is the same as `Just 0`,
  -- so we need to compare their actual version numbers.
  compare v0@(Version (Just 0) _ _) v1@(Version Nothing _ _) = compare (wipe v0) v1
  compare v0@(Version Nothing _ _) v1@(Version (Just 0) _ _) = compare v0 (wipe v1)

  compare (Version (Just _) _ _) (Version Nothing _ _) = GT
  compare (Version Nothing _ _) (Version (Just _) _ _) = LT

  -- | If two epochs are equal, we need to compare their actual version numbers.
  -- Otherwise, the comparison of the epochs is the only thing that matters.
  compare v0@(Version (Just n) _ _) v1@(Version (Just m) _ _) | n == m = compare (wipe v0) (wipe v1)
                                                              | otherwise = compare n m

  -- | If the two Versions were otherwise equal and recursed down this far,
  -- we need to compare them by their "release" values.
  compare (Version _ [] rs) (Version _ [] rs') = compare (Version Nothing rs []) (Version Nothing rs' [])

  -- | If one side has run out of chunks to compare but the other hasn't,
  -- the other must be newer.
  compare (Version _ _ _)  (Version _ [] _) = GT
  compare (Version _ [] _) (Version _ _ _)  = LT

  -- | The usual case. If first VChunks of each Version is equal, then we
  -- keep recursing. Otherwise, we don't need to check further. Consider @1.2@
  -- compared to @1.1.3.4.5.6@.
  compare (Version _ (a:as) rs) (Version _ (b:bs) rs') = case f a b of
    EQ  -> compare (Version Nothing as rs) (Version Nothing bs rs')
    res -> res
    where f [] [] = EQ

          -- | Opposite of the above. If we've recursed this far and one side has
          -- fewer chunks, it must be the "greater" version. A Chunk break only occurs in
          -- a switch from digits to letters and vice versa, so anything "extra" must be
          -- an @rc@ marking or similar. Consider @1.1@ compared to @1.1rc1@.
          f [] _  = GT
          f _ []  = LT

          -- | The usual case.
          f (Digits n:ns) (Digits m:ms) | n > m = GT
                                        | n < m = LT
                                        | otherwise = f ns ms
          f (Str n:ns) (Str m:ms) | n > m = GT
                                  | n < m = LT
                                  | otherwise = f ns ms

          -- | An arbitrary decision to prioritize digits over letters.
          f (Digits _ :_) (Str _ :_) = GT
          f (Str _ :_ ) (Digits _ :_) = LT

-- | > vEpoch :: Lens' Version (Maybe Int)
vEpoch :: Functor f => (Maybe Int -> f (Maybe Int)) -> Version -> f Version
vEpoch f v = fmap (\ve -> v { _vEpoch = ve }) (f $ _vEpoch v)

-- | > vChunks :: Lens' Version [VChunk]
vChunks :: Functor f => ([VChunk] -> f [VChunk]) -> Version -> f Version
vChunks f v = fmap (\vc -> v { _vChunks = vc }) (f $ _vChunks v)
{-# INLINE vChunks #-}

-- | > vRel :: Lens' Version [VChunk]
vRel :: Functor f => ([VChunk] -> f [VChunk]) -> Version -> f Version
vRel f v = fmap (\vr -> v { _vRel = vr }) (f $ _vRel v)
{-# INLINE vRel #-}

-- | A (Complex) Mess.
-- This is a /descriptive/ parser, based on examples of stupidly
-- crafted version numbers used in the wild.
--
-- Groups of letters/numbers, separated by a period, can be
-- further separated by the symbols @_-+:@
--
-- Unfortunately, @VChunk@s cannot be used here, as some developers have
-- numbers like @1.003.04@ which make parsers quite sad.
--
-- Not guaranteed to have well-defined ordering (@Ord@) behaviour,
-- but so far internal tests show consistency.
data Mess = VLeaf [Text] | VNode [Text] VSep Mess deriving (Eq,Show,Generic,NFData,Hashable)

instance Ord Mess where
  compare (VLeaf l1) (VLeaf l2)     = compare l1 l2
  compare (VNode t1 _ _) (VLeaf t2) = compare t1 t2
  compare (VLeaf t1) (VNode t2 _ _) = compare t1 t2
  compare (VNode t1 _ v1) (VNode t2 _ v2) | t1 < t2 = LT
                                          | t1 > t2 = GT
                                          | otherwise = compare v1 v2

-- | Developers use a number of symbols to seperate groups of digits/letters
-- in their version numbers. These are:
--
-- * A colon (:). Often denotes an "epoch".
-- * A hyphen (-).
-- * A plus (+). Stop using this outside of metadata if you are. Example: @10.2+0.93+1-1@
-- * An underscore (_). Stop using this if you are.
data VSep = VColon | VHyphen | VPlus | VUnder deriving (Eq,Show,Generic,NFData,Hashable)

-- | A synonym for the more verbose `megaparsec` error type.
type ParsingError = ParseError (Token Text) Void

-- | A wrapper for a parser function. Can be composed via their
-- Monoid instance, such that a different parser can be tried
-- if a previous one fails.
newtype VParser = VParser { runVP :: Text -> Either ParsingError Versioning }

instance Monoid VParser where
  -- | A parser which will always fail.
  mempty = VParser $ \_ -> Ideal <$> semver ""

  -- | Will attempt the right parser if the left one fails.
  (VParser f) `mappend` (VParser g) = VParser h
    where h t = either (const (g t)) Right $ f t

-- | Parse a piece of @Text@ into either an (Ideal) SemVer, a (General)
-- Version, or a (Complex) Mess.
parseV :: Text -> Either ParsingError Versioning
parseV = runVP $ semverP <> versionP <> messP

-- | A wrapped `SemVer` parser. Can be composed with other parsers.
semverP :: VParser
semverP = VParser $ fmap Ideal . semver

-- | Parse a (Ideal) Semantic Version.
semver :: Text -> Either ParsingError SemVer
semver = parse (semver' <* eof) "Semantic Version"

-- | Internal megaparsec parser of 'semverP'.
semver' :: Parsec Void Text SemVer
semver' = SemVer <$> major <*> minor <*> patch <*> preRel <*> metaData

-- | Parse a group of digits, which can't be lead by a 0, unless it is 0.
digits :: Parsec Void Text Int
digits = read <$> ((unpack <$> string "0") <|> some digitChar)

major :: Parsec Void Text Int
major = digits <* char '.'

minor :: Parsec Void Text Int
minor = major

patch :: Parsec Void Text Int
patch = digits

preRel :: Parsec Void Text [VChunk]
preRel = (char '-' *> chunks) <|> pure []

metaData :: Parsec Void Text [VChunk]
metaData = (char '+' *> chunks) <|> pure []

chunks :: Parsec Void Text [VChunk]
chunks = chunk `sepBy` char '.'

-- | Handling @0@ is a bit tricky. We can't allow runs of zeros in a chunk,
-- since a version like @1.000.1@ would parse as @1.0.1@.
chunk :: Parsec Void Text VChunk
chunk = try zeroWithLetters <|> oneZero <|> many (iunit <|> sunit)
  where oneZero = (:[]) . Digits . read . unpack <$> string "0"
        zeroWithLetters = do
          z <- Digits . read . unpack <$> string "0"
          s <- some sunit
          c <- chunk
          pure $ (z : s) ++ c

iunit :: Parsec Void Text VUnit
iunit = Digits . read <$> some digitChar

sunit :: Parsec Void Text VUnit
sunit = Str . pack <$> some letterChar

-- | A wrapped `Version` parser. Can be composed with other parsers.
versionP :: VParser
versionP = VParser $ fmap General . version

-- | Parse a (General) `Version`, as defined above.
version :: Text -> Either ParsingError Version
version = parse (version' <* eof) "Version"

-- | Internal megaparsec parser of 'versionP'.
version' :: Parsec Void Text Version
version' = Version <$> optional (try epoch) <*> chunks <*> preRel

epoch :: Parsec Void Text Int
epoch = read <$> (some digitChar <* char ':')

-- | A wrapped `Mess` parser. Can be composed with other parsers.
messP :: VParser
messP = VParser $ fmap Complex . mess

-- | Parse a (Complex) `Mess`, as defined above.
mess :: Text -> Either ParsingError Mess
mess = parse (mess' <* eof) "Mess"

-- | Internal megaparsec parser of 'messP'.
mess' :: Parsec Void Text Mess
mess' = try node <|> leaf

leaf :: Parsec Void Text Mess
leaf = VLeaf <$> tchunks

node :: Parsec Void Text Mess
node = VNode <$> tchunks <*> sep <*> mess'

tchunks :: Parsec Void Text [Text]
tchunks = (pack <$> some (letterChar <|> digitChar)) `sepBy` char '.'

sep :: Parsec Void Text VSep
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
prettyVer (Version ep cs pr) = ep' <> mconcat (ver <> pr')
  where ver = intersperse "." $ chunksAsT cs
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)
        ep' = maybe "" (\e -> showt e <> ":") ep

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

-- | Flip an Ordering.
opposite :: Ordering -> Ordering
opposite EQ = EQ
opposite LT = GT
opposite GT = LT

-- Yes, `text-show` exists, but this reduces external dependencies.
showt :: Show a => a -> Text
showt = pack . show
