{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}

-- |
-- Module    : Data.Versions
-- Copyright : (c) Colin Woodbury, 2015 - 2023
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- A library for parsing and comparing software version numbers.
--
-- We like to give version numbers to our software in a myriad of different
-- ways. Some ways follow strict guidelines for incrementing and comparison.
-- Some follow conventional wisdom and are generally self-consistent. Some are
-- just plain asinine. This library provides a means of parsing and comparing
-- /any/ style of versioning, be it a nice Semantic Version like this:
--
-- > 1.2.3-r1+git123
--
-- ...or a monstrosity like this:
--
-- > 2:10.2+0.0093r3+1-1
--
-- Please switch to <http://semver.org Semantic Versioning> if you aren't
-- currently using it. It provides consistency in version incrementing and has
-- the best constraints on comparisons.
--
-- __This library implements version @2.0.0@ of the SemVer spec.__
--
-- == Using the Parsers
-- In general, `versioning` is the function you want. It attempts to parse a
-- given `Text` using the three individual parsers, `semver`, `version` and
-- `mess`. If one fails, it tries the next. If you know you only want to parse
-- one specific version type, use that parser directly (e.g. `semver`).

module Data.Versions
  ( -- * Types
    Versioning(..), isIdeal, isGeneral, isComplex
  , SemVer(..)
  , PVP(..)
  , Version(..)
  , Mess(..), messMajor, messMinor, messPatch, messPatchChunk
  , MChunk(..)
  , VUnit(..), digits, str
  , VChunk
  , VSep(..)
    -- * Parsing Versions
  , ParsingError
  , versioning, semver, pvp, version, mess
    -- ** Megaparsec Parsers
    -- | For when you'd like to mix version parsing into some larger parser.
  , versioning', semver', pvp', version', mess'
    -- * Pretty Printing
  , prettyV, prettySemVer, prettyPVP, prettyVer, prettyMess, errorBundlePretty
    -- * Lenses
  , Lens'
  , Traversal'
  , Semantic(..)
    -- ** Traversing Text
    -- | When traversing `Text`, leveraging its `Semantic` instance will
    -- likely benefit you more than using these Traversals directly.
  , _Versioning, _SemVer, _Version, _Mess
    -- ** Versioning Traversals
  , _Ideal, _General, _Complex
    -- ** (General) Version Lenses
  , epoch
    -- ** Misc. Lenses / Traversals
  , _Digits, _Str
  ) where

import qualified Control.Applicative.Combinators.NonEmpty as PC
import           Control.DeepSeq
import           Control.Monad (unless, void, when)
import           Data.Bool (bool)
import           Data.Char (isAlpha, isAlphaNum)
import           Data.Foldable (fold)
import           Data.Hashable (Hashable)
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           Text.Megaparsec hiding (chunk)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char.Lexer (decimal)

---

-- | A top-level Versioning type. Acts as a wrapper for the more specific types.
-- This allows each subtype to have its own parser, and for said parsers to be
-- composed. This is useful for specifying custom behaviour for when a certain
-- parser fails.
data Versioning = Ideal SemVer | General Version | Complex Mess
  deriving (Eq, Show, Generic, NFData, Hashable)

-- | Short-hand for detecting a `SemVer`.
isIdeal :: Versioning -> Bool
isIdeal (Ideal _) = True
isIdeal _         = False

-- | Short-hand for detecting a `Version`.
isGeneral :: Versioning -> Bool
isGeneral (General _) = True
isGeneral _           = False

-- | Short-hand for detecting a `Mess`.
isComplex :: Versioning -> Bool
isComplex (Complex _) = True
isComplex _           = False

-- | Comparison of @Ideal@s is always well defined.
--
-- If comparison of @General@s is well-defined, then comparison of @Ideal@ and
-- @General@ is well-defined, as there exists a perfect mapping from @Ideal@ to
-- @General@.
--
-- If comparison of @Complex@es is well-defined, then comparison of @General@
-- and @Complex@ is well defined for the same reason. This implies comparison of
-- @Ideal@ and @Complex@ is also well-defined.
instance Ord Versioning where
  compare (Ideal s)     (Ideal s')   = compare s s'
  compare (General v)   (General v') = compare v v'
  compare (Complex m)   (Complex m') = compare m m'
  compare (Ideal s)     (General v)  = compare (vFromS s) v
  compare (General v)   (Ideal s)    = opposite $ compare (vFromS s) v
  compare (General v)   (Complex m)  = compare (mFromV v) m
  compare (Complex m)   (General v)  = opposite $ compare (mFromV v) m
  compare (Ideal s)     (Complex m)  = semverAndMess s m
  compare (Complex m)   (Ideal s)    = opposite $ semverAndMess s m

-- | Convert a `SemVer` to a `Version`.
vFromS :: SemVer -> Version
vFromS (SemVer ma mi pa re me) =
  Version
  { _vEpoch = Nothing
  , _vChunks = Chunks $ Numeric ma :| [Numeric mi, Numeric pa]
  , _vMeta = me
  , _vRel = re }

-- | Convert a `Version` to a `Mess`.
mFromV :: Version -> Mess
mFromV (Version me (Chunks v) r _) = case me of
  Nothing -> f
  Just e  ->
    let cs = NEL.singleton . MDigit e $ showt e
    in Mess cs $ Just (VColon, f)
  where
    f :: Mess
    f = Mess cs $ fmap g r
      where
        cs = NEL.map toMChunk v

    g :: Release -> (VSep, Mess)
    g (Release cs) = (VHyphen, Mess ms Nothing)
      where
        ms = NEL.map toMChunk cs

-- | Special logic for when semver-like values can be extracted from a `Mess`.
-- This avoids having to "downcast" the `SemVer` into a `Mess` before comparing,
-- and in some cases can offer better comparison results.
semverAndMess :: SemVer -> Mess -> Ordering
semverAndMess s@(SemVer ma mi pa _ _) m = case compare ma <$> messMajor m of
  Nothing -> fallback
  Just LT -> LT
  Just GT -> GT
  Just EQ -> case compare mi <$> messMinor m of
    Nothing -> fallback
    Just LT -> LT
    Just GT -> GT
    Just EQ -> case compare pa <$> messPatch m of
      Just LT -> LT
      Just GT -> GT
      -- If they've been equal up to this point, the `Mess`
      -- will by definition have more to it, meaning that
      -- it's more likely to be newer, despite its poor shape.
      Just EQ -> fallback
      Nothing -> case messPatchChunk m of
        Nothing             -> fallback
        Just (Digits pa':|_) -> case compare pa pa' of
          LT -> LT
          GT -> GT
          EQ -> GT  -- This follows semver's rule!
        Just _ -> fallback
  where
    fallback :: Ordering
    fallback = compare (General $ vFromS s) (Complex m)

instance Semantic Versioning where
  major f (Ideal v)   = Ideal   <$> major f v
  major f (General v) = General <$> major f v
  major f (Complex v) = Complex <$> major f v
  {-# INLINE major #-}

  minor f (Ideal v)   = Ideal   <$> minor f v
  minor f (General v) = General <$> minor f v
  minor f (Complex v) = Complex <$> minor f v
  {-# INLINE minor #-}

  patch f (Ideal v)   = Ideal   <$> patch f v
  patch f (General v) = General <$> patch f v
  patch f (Complex v) = Complex <$> patch f v
  {-# INLINE patch #-}

  release f (Ideal v)   = Ideal   <$> release f v
  release f (General v) = General <$> release f v
  release f (Complex v) = Complex <$> release f v
  {-# INLINE release #-}

  meta f (Ideal v)   = Ideal   <$> meta f v
  meta f (General v) = General <$> meta f v
  meta f (Complex v) = Complex <$> meta f v
  {-# INLINE meta #-}

  semantic f (Ideal v)   = Ideal   <$> semantic f v
  semantic f (General v) = General <$> semantic f v
  semantic f (Complex v) = Complex <$> semantic f v
  {-# INLINE semantic #-}

-- | Traverse some Text for its inner versioning.
--
-- @
-- λ "1.2.3" & _Versioning . _Ideal . patch %~ (+ 1)  -- or just: "1.2.3" & patch %~ (+ 1)
-- "1.2.4"
-- @
_Versioning :: Traversal' Text Versioning
_Versioning f t = either (const (pure t)) (fmap prettyV . f) $ versioning t
{-# INLINE _Versioning #-}

-- | Traverse some Text for its inner SemVer.
_SemVer :: Traversal' Text SemVer
_SemVer f t = either (const (pure t)) (fmap prettySemVer . f) $ semver t
{-# INLINE _SemVer #-}

-- | Traverse some Text for its inner Version.
_Version :: Traversal' Text Version
_Version f t = either (const (pure t)) (fmap prettyVer . f) $ version t
{-# INLINE _Version #-}

-- | Traverse some Text for its inner Mess.
_Mess :: Traversal' Text Mess
_Mess f t = either (const (pure t)) (fmap prettyMess . f) $ mess t
{-# INLINE _Mess #-}

-- | Possibly extract a `SemVer` from a `Versioning`.
_Ideal :: Traversal' Versioning SemVer
_Ideal f (Ideal s) = Ideal <$> f s
_Ideal _ v         = pure v
{-# INLINE _Ideal #-}

-- | Possibly extract a `Version` from a `Versioning`.
_General :: Traversal' Versioning Version
_General f (General v) = General <$> f v
_General _ v           = pure v
{-# INLINE _General #-}

-- | Possibly extract a `Mess` from a `Versioning`.
_Complex :: Traversal' Versioning Mess
_Complex f (Complex m) = Complex <$> f m
_Complex _ v           = pure v
{-# INLINE _Complex #-}

-- | Simple Lenses compatible with both lens and microlens.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Simple Traversals compatible with both lens and microlens.
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- | Version types which sanely and safely yield `SemVer`-like information about
-- themselves. For instances other than `SemVer` itself however, these optics
-- may /not/ yield anything, depending on the actual value being traversed.
-- Hence, the optics here are all `Traversal'`s.
--
-- Consider the `Version` @1.2.3.4.5@. We can imagine wanting to increment the
-- minor number:
--
-- @
-- λ "1.2.3.4.5" & minor %~ (+ 1)
-- "1.3.3.4.5"
-- @
--
-- But of course something like this would fail:
--
-- @
-- λ "1.e.3.4.5" & minor %~ (+ 1)
-- "1.e.3.4.5"
-- @
--
-- However!
--
-- @
-- λ "1.e.3.4.5" & major %~ (+ 1)
-- "2.e.3.4.5"
-- @
class Semantic v where
  -- | @MAJOR.minor.patch-prerel+meta@
  major    :: Traversal' v Word
  -- | @major.MINOR.patch-prerel+meta@
  minor    :: Traversal' v Word
  -- | @major.minor.PATCH-prerel+meta@
  patch    :: Traversal' v Word
  -- | @major.minor.patch-PREREL+meta@
  release  :: Traversal' v (Maybe Release)
  -- | @major.minor.patch-prerel+META@
  meta     :: Traversal' v (Maybe Text)
  -- | A Natural Transformation into an proper `SemVer`.
  semantic :: Traversal' v SemVer

instance Semantic Text where
  major    = _Versioning . major
  minor    = _Versioning . minor
  patch    = _Versioning . patch
  release  = _Versioning . release
  meta     = _Versioning . meta
  semantic = _SemVer

--------------------------------------------------------------------------------
-- (Ideal) SemVer

-- | An (Ideal) version number that conforms to Semantic Versioning.
-- This is a /prescriptive/ parser, meaning it follows the SemVer standard.
--
-- Legal semvers are of the form: MAJOR.MINOR.PATCH-PREREL+META
--
-- Example: @1.2.3-r1+commithash@
--
-- Extra Rules:
--
-- 1. Pre-release versions have /lower/ precedence than normal versions.
--
-- 2. Build metadata does not affect version precedence.
--
-- 3. PREREL and META strings may only contain ASCII alphanumerics and hyphens.
--
-- For more information, see http://semver.org
data SemVer = SemVer
  { _svMajor  :: !Word
  , _svMinor  :: !Word
  , _svPatch  :: !Word
  , _svPreRel :: !(Maybe Release)
  , _svMeta   :: !(Maybe Text) }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Hashable)

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
     EQ -> compare pr pr'

instance Semantic SemVer where
  major f sv = fmap (\ma -> sv { _svMajor = ma }) (f $ _svMajor sv)
  {-# INLINE major #-}

  minor f sv = fmap (\mi -> sv { _svMinor = mi }) (f $ _svMinor sv)
  {-# INLINE minor #-}

  patch f sv = fmap (\pa -> sv { _svPatch = pa }) (f $ _svPatch sv)
  {-# INLINE patch #-}

  release f sv = fmap (\pa -> sv { _svPreRel = pa }) (f $ _svPreRel sv)
  {-# INLINE release #-}

  meta f sv = fmap (\pa -> sv { _svMeta = pa }) (f $ _svMeta sv)
  {-# INLINE meta #-}

  semantic = ($)
  {-# INLINE semantic #-}

-- | `Chunk`s have comparison behaviour according to SemVer's rules for preleases.
newtype Release = Release (NonEmpty Chunk)
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, Hashable)

instance Ord Release where
  compare (Release as) (Release bs) =
    fromMaybe EQ . listToMaybe . mapMaybe f $ zipLongest (NEL.toList as) (NEL.toList bs)
    where
      f :: These Chunk Chunk -> Maybe Ordering
      f (Both a b) = case cmpSemVer a b of
        LT -> Just LT
        GT -> Just GT
        EQ -> Nothing
      f (This _)   = Just GT
      f (That _)   = Just LT

-- | A logical unit of a version number.
--
-- Either entirely numerical (with no leading zeroes) or entirely alphanumerical
-- (with a free mixture of numbers, letters, and hyphens.)
--
-- Groups of these (like `Release`) are separated by periods to form a full
-- section of a version number.
--
-- Examples:
--
-- @
-- 1
-- 20150826
-- r3
-- 0rc1-abc3
-- @
data Chunk = Numeric Word | Alphanum Text
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, Hashable)

toMChunk :: Chunk -> MChunk
toMChunk (Numeric n)  = MDigit n $ showt n
toMChunk (Alphanum s) = MPlain s

-- | `Chunk` is used in multiple places but requires different comparison
-- semantics depending on the wrapping type. This function and `cmpLenient`
-- below provide this.
cmpSemVer :: Chunk -> Chunk -> Ordering
cmpSemVer (Numeric a) (Numeric b)   = compare a b
cmpSemVer (Numeric _) (Alphanum _)  = LT
cmpSemVer (Alphanum _) (Numeric _)  = GT
cmpSemVer (Alphanum a) (Alphanum b) = compare a b

-- | Like `cmpSemVer`, but for `Version`s. We need to be mindful of comparisons
-- like @1.2.0 > 1.2.0rc1@ which normally wouldn't occur in SemVer.
cmpLenient :: Chunk -> Chunk -> Ordering
cmpLenient (Numeric a) (Numeric b)       = compare a b
cmpLenient a@(Alphanum x) b@(Alphanum y) =
  case (singleDigitLenient a, singleDigitLenient b) of
    (Just i, Just j) -> compare i j
    _                -> compare x y
cmpLenient (Numeric n) b@(Alphanum _) =
  case singleDigitLenient b of
    Nothing -> GT
    Just m -> case compare n m of
      -- 1.2.0 > 1.2.0rc1
      EQ -> GT
      c  -> c
cmpLenient a@(Alphanum _) (Numeric n) =
  case singleDigitLenient a of
    Nothing -> LT
    Just m -> case compare m n of
      -- 1.2.0rc1 < 1.2.0
      EQ -> LT
      c  -> c

-- | Like `singleDigit` but will grab a leading `Word` even if followed by
-- letters.
singleDigitLenient :: Chunk -> Maybe Word
singleDigitLenient (Numeric n)  = Just n
singleDigitLenient (Alphanum s) = hush $ parse unsignedP "Single Digit Lenient" s

-- | A single unit of a Version. May be digits or a string of characters. Groups
-- of these are called `VChunk`s, and are the identifiers separated by periods
-- in the source.
data VUnit = Digits Word | Str Text
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (NFData, Hashable)

-- | Smart constructor for a `VUnit` made of digits.
digits :: Word -> VUnit
digits = Digits

-- | Smart constructor for a `VUnit` made of letters.
str :: Text -> Maybe VUnit
str t = bool Nothing (Just $ Str t) $ T.all isAlpha t

-- | Possibly traverse the inner digit value of a `VUnit`.
_Digits :: Traversal' VUnit Word
_Digits f (Digits i) = Digits <$> f i
_Digits _ v          = pure v
{-# INLINE _Digits #-}

-- | Possibly traverse the inner text of a `VUnit`.
_Str :: Traversal' VUnit Text
_Str f (Str t) = Str . (\t' -> bool t t' (T.all isAlpha t')) <$> f t
_Str _ v       = pure v
{-# INLINE _Str #-}

-- | A logical unit of a version number. Can consist of multiple letters
-- and numbers.
type VChunk = NonEmpty VUnit

--------------------------------------------------------------------------------
-- (Haskell) PVP

-- | A PVP version number specific to the Haskell ecosystem. Like SemVer this is
-- a prescriptive scheme, and follows <https://pvp.haskell.org/ the PVP spec>.
--
-- Legal PVP values are of the form: MAJOR(.MAJOR.MINOR)
--
-- Example: @1.2.3@
--
-- Extra Rules:
--
-- 1. Each component must be a number.
--
-- 2. Only the first MAJOR component is actually necessary. Otherwise, there can
--    be any number of components. @1.2.3.4.5.6.7@ is legal.
--
-- 3. Unlike SemVer there are two MAJOR components, and both indicate a breaking
--    change. The spec otherwise designates no special meaning to components
--    past the MINOR position.
newtype PVP = PVP { _pComponents :: NonEmpty Word }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance Semantic PVP where
  major f (PVP (m :| rs)) = (\ma -> PVP $ ma :| rs) <$> f m
  {-# INLINE major #-}

  minor f (PVP (m :| mi : rs)) = (\mi' -> PVP $ m :| mi' : rs) <$> f mi
  minor f (PVP (m :| []))      = (\mi' -> PVP $ m :| [mi']) <$> f 0
  {-# INLINE minor #-}

  patch f (PVP (m :| mi : pa : rs)) = (\pa' -> PVP $ m :| mi : pa' : rs) <$> f pa
  patch f (PVP (m :| [mi]))         = (\pa' -> PVP $ m :| mi : [pa']) <$> f 0
  patch f (PVP (m :| []))           = (\pa' -> PVP $ m :| 0 : [pa']) <$> f 0
  {-# INLINE patch #-}

  release f p = p <$ f Nothing
  {-# INLINE release #-}

  meta f p = p <$ f Nothing
  {-# INLINE meta #-}

  semantic f (PVP (m :| rs)) = (\(SemVer ma mi pa _ _) -> PVP $ ma :| [mi, pa]) <$> f s
    where
      s = case rs of
        mi : pa : _ -> SemVer m mi pa Nothing Nothing
        mi : _      -> SemVer m mi 0  Nothing Nothing
        []          -> SemVer m 0 0   Nothing Nothing
  {-# INLINE semantic #-}

--------------------------------------------------------------------------------
-- (General) Version

-- | A version number with decent structure and comparison logic.
--
-- This is a /descriptive/ scheme, meaning that it encapsulates the most common,
-- unconscious patterns that developers use when assigning version numbers to
-- their software. If not `SemVer`, most version numbers found in the wild will
-- parse as a `Version`. These generally conform to the @x.x.x-x@ pattern, and
-- may optionally have an /epoch/.
--
-- Epochs are prefixes marked by a colon, like in @1:2.3.4@. When comparing two
-- `Version` values, epochs take precedent. So @2:1.0.0 > 1:9.9.9@. If one of
-- the given `Version`s has no epoch, its epoch is assumed to be 0.
--
-- Examples of @Version@ that are not @SemVer@: 0.25-2, 8.u51-1, 20150826-1,
-- 1:2.3.4
data Version = Version
  { _vEpoch  :: !(Maybe Word)
  , _vChunks :: !Chunks
  , _vRel    :: !(Maybe Release)
  , _vMeta   :: !(Maybe Text) }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

-- | Customized. As in SemVer, metadata is ignored for the purpose of
-- comparison.
instance Ord Version where
  -- If two epochs are equal, we need to compare their actual version numbers.
  -- Otherwise, the comparison of the epochs is the only thing that matters.
  compare (Version mae ac ar _) (Version mbe bc br _) =
    case compare ae be of
      EQ -> case compare ac bc of
        EQ  -> compare ar br
        ord -> ord
      ord -> ord
    where
      ae = fromMaybe 0 mae
      be = fromMaybe 0 mbe

instance Semantic Version where
  major f (Version e (Chunks (Numeric n :| cs)) me rs) =
    (\n' -> Version e (Chunks $ Numeric n' :| cs) me rs) <$> f n
  major _ v = pure v
  {-# INLINE major #-}

  minor f (Version e (Chunks (c :| Numeric n : cs)) me rs) =
    (\n' -> Version e (Chunks $ c :| Numeric n' : cs) me rs) <$> f n
  minor _ v = pure v
  {-# INLINE minor #-}

  patch f (Version e (Chunks (c :| d : Numeric n : cs)) me rs) =
    (\n' -> Version e (Chunks $ c :| d : Numeric n' : cs) me rs) <$> f n
  patch _ v = pure v
  {-# INLINE patch #-}

  -- | This will always succeed.
  release f v = fmap (\vr -> v { _vRel = vr }) (f $ _vRel v)
  {-# INLINE release #-}

  -- | This will always fail.
  meta _ v = pure v
  {-# INLINE meta #-}

  semantic f (Version _ (Chunks (Numeric a :| Numeric b : Numeric c : _)) rs me) =
    vFromS <$> f (SemVer a b c rs me)
  semantic _ v = pure v
  {-# INLINE semantic #-}

-- | A `Version`'s inner epoch `Word`.
epoch :: Lens' Version (Maybe Word)
epoch f v = fmap (\ve -> v { _vEpoch = ve }) (f $ _vEpoch v)
{-# INLINE epoch #-}

-- | `Chunk`s that have a comparison behaviour specific to `Version`.
newtype Chunks = Chunks (NonEmpty Chunk)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance Ord Chunks where
  compare (Chunks as) (Chunks bs) =
    fromMaybe EQ . listToMaybe . mapMaybe f $ zipLongest (NEL.toList as) (NEL.toList bs)
    where
      f :: These Chunk Chunk -> Maybe Ordering
      f (Both a b) = case cmpLenient a b of
        LT -> Just LT
        GT -> Just GT
        EQ -> Nothing
      f (This _)   = Just GT
      f (That _)   = Just LT

--------------------------------------------------------------------------------
-- (Complex) Mess

-- | Possible values of a section of a `Mess`. A numeric value is extracted if
-- it could be, alongside the original text it came from. This preserves both
-- `Ord` and pretty-print behaviour for versions like @1.003.0@.
data MChunk
  = MDigit Word Text
  -- ^ A nice numeric value.
  | MRev Word Text
  -- ^ A numeric value preceeded by an @r@, indicating a revision.
  | MPlain Text
  -- ^ Anything else.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance Ord MChunk where
  compare (MDigit a _) (MDigit b _) = compare a b
  compare (MRev a _) (MRev b _)     = compare a b
  compare (MPlain a) (MPlain b)     = compare a b
  compare a b                       = compare (mchunkText a) (mchunkText b)

-- | A total extraction of the `Text` from an `MChunk`.
mchunkText :: MChunk -> Text
mchunkText (MDigit _ t) = t
mchunkText (MRev _ t)   = t
mchunkText (MPlain t)   = t

-- | A (Complex) Mess. This is a /descriptive/ parser, based on examples of
-- stupidly crafted version numbers used in the wild.
--
-- Groups of letters/numbers, separated by a period, can be further separated by
-- the symbols @_-+:@
--
-- Some `Mess` values have a shape that is tantalizingly close to a `SemVer`.
-- Example: @1.6.0a+2014+m872b87e73dfb-1@. For values like these, we can extract
-- the semver-compatible values out with `messMajor`, etc.
--
-- Not guaranteed to have well-defined ordering (@Ord@) behaviour, but so far
-- internal tests show consistency. `messMajor`, etc., are used internally where
-- appropriate to enhance accuracy.
data Mess = Mess !(NonEmpty MChunk) !(Maybe (VSep, Mess))
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

-- | Try to extract the "major" version number from `Mess`, as if it were a
-- `SemVer`.
messMajor :: Mess -> Maybe Word
messMajor (Mess (MDigit i _ :| _) _) = Just i
messMajor _                          = Nothing

-- | Try to extract the "minor" version number from `Mess`, as if it were a
-- `SemVer`.
messMinor :: Mess -> Maybe Word
messMinor (Mess (_ :| MDigit i _ : _) _) = Just i
messMinor _                              = Nothing

-- | Try to extract the "patch" version number from `Mess`, as if it were a
-- `SemVer`.
messPatch :: Mess -> Maybe Word
messPatch (Mess (_ :| _ : MDigit i _ : _) _) = Just i
messPatch _                                  = Nothing

-- | Okay, fine, say `messPatch` couldn't find a nice value. But some `Mess`es
-- have a "proper" patch-plus-release-candidate value in their patch position,
-- which is parsable as a `VChunk`.
--
-- Example: @1.6.0a+2014+m872b87e73dfb-1@ We should be able to extract @0a@ safely.
messPatchChunk :: Mess -> Maybe VChunk
messPatchChunk (Mess (_ :| _ : MPlain p : _) _) = hush $ parse (chunkWith unit) "Chunk" p
messPatchChunk _                                = Nothing

instance Ord Mess where
  compare (Mess t1 m1) (Mess t2 m2) = case compare t1 t2 of
    EQ  -> case (m1, m2) of
      (Just (_, v1), Just (_, v2)) -> compare v1 v2
      (Just (_, _), Nothing)       -> GT
      (Nothing, Just (_, _))       -> LT
      (Nothing, Nothing)           -> EQ
    res -> res

instance Semantic Mess where
  major f (Mess (MDigit n _ :| ts) m) = (\n' -> Mess (MDigit n' (showt n') :| ts) m) <$> f n
  major _ v = pure v
  {-# INLINE major #-}

  minor f (Mess (t0 :| MDigit n _ : ts) m) = (\n' -> Mess (t0 :| MDigit n' (showt n') : ts) m) <$> f n
  minor _ v = pure v
  {-# INLINE minor #-}

  patch f (Mess (t0 :| t1 : MDigit n _ : ts) m) = (\n' -> Mess (t0 :| t1 : MDigit n' (showt n') : ts) m) <$> f n
  patch _ v = pure v
  {-# INLINE patch #-}

  -- | This will always fail.
  release _ v = pure v
  {-# INLINE release #-}

  -- | This will always fail.
  meta _ v = pure v
  {-# INLINE meta #-}

  -- | Good luck.
  semantic f (Mess (MDigit t0 _ :| MDigit t1 _ : MDigit t2 _ : _) _) =
    mFromV . vFromS <$> f (SemVer t0 t1 t2 Nothing Nothing)
  semantic _ v = pure v
  {-# INLINE semantic #-}

-- | Developers use a number of symbols to seperate groups of digits/letters in
-- their version numbers. These are:
--
-- * A colon (:). Often denotes an "epoch".
-- * A hyphen (-).
-- * A tilde (~). Example: @12.0.0-3ubuntu1~20.04.5@
-- * A plus (+). Stop using this outside of metadata if you are. Example: @10.2+0.93+1-1@
-- * An underscore (_). Stop using this if you are.
data VSep = VColon | VHyphen | VPlus | VUnder | VTilde
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

--------------------------------------------------------------------------------
-- Parsing

-- | A synonym for the more verbose 'megaparsec' error type.
type ParsingError = ParseErrorBundle Text Void

-- | Parse a piece of `Text` into either an (Ideal) `SemVer`, a (General)
-- `Version`, or a (Complex) `Mess`.
versioning :: Text -> Either ParsingError Versioning
versioning = parse versioning' "versioning"

-- | Parse a `Versioning`. Assumes the version number is the last token in
-- the string.
versioning' :: Parsec Void Text Versioning
versioning' = choice [ try (fmap Ideal semver''    <* eof)
                     , try (fmap General version'' <* eof)
                     , fmap Complex mess''         <* eof ]

-- | Parse a (Ideal) Semantic Version.
semver :: Text -> Either ParsingError SemVer
semver = parse (semver'' <* eof) "Semantic Version"

-- | Internal megaparsec parser of `semver`.
semver' :: Parsec Void Text SemVer
semver' = L.lexeme space semver''

semver'' :: Parsec Void Text SemVer
semver'' = SemVer <$> majorP <*> minorP <*> patchP <*> optional releaseP <*> optional metaData

-- | Parse a group of digits, which can't be lead by a 0, unless it is 0.
unsignedP :: Parsec Void Text Word
unsignedP = (0 <$ char '0') <|> decimal

majorP :: Parsec Void Text Word
majorP = unsignedP <* char '.'

minorP :: Parsec Void Text Word
minorP = majorP

patchP :: Parsec Void Text Word
patchP = unsignedP

releaseP :: Parsec Void Text Release
releaseP = char '-' *> fmap Release (chunkP `PC.sepBy1` char '.')

chunkP :: Parsec Void Text Chunk
chunkP = alphanumP <|> numericP

alphanumP :: Parsec Void Text Chunk
alphanumP = do
  ids <- takeWhile1P (Just "Hyphenated Alphanums") (\c -> isAlphaNum c || c == '-')
  -- It's okay for this to `fail` like this, since this fail is caught higher up
  -- in `chunkP` and another parser which should be guaranteed to succeed is
  -- called. It's guaranteed since by this point we /did/ parse something, but
  -- the test below proves it contains only numbers. Therefore the fallback call
  -- to `numericP` should succeed.
  unless (T.any (\c -> isAlpha c || c == '-') ids) $ fail "Only numeric!"
  pure $ Alphanum ids

alphanumWithoutHyphensP :: Parsec Void Text Chunk
alphanumWithoutHyphensP = do
  ids <- takeWhile1P (Just "Unhyphenated Alphanums") isAlphaNum
  unless (T.any isAlpha ids) $ fail "Only numeric!"
  pure $ Alphanum ids

numericP :: Parsec Void Text Chunk
numericP = Numeric <$> unsignedP

chunkWithoutHyphensP :: Parsec Void Text Chunk
chunkWithoutHyphensP = alphanumWithoutHyphensP <|> numericP

preRel :: Parsec Void Text [VChunk]
preRel = (char '-' *> vchunks) <|> pure []

metaData :: Parsec Void Text Text
metaData = do
  void $ char '+'
  fold . NEL.intersperse "." <$> section `PC.sepBy1` char '.'
  where
    section :: Parsec Void Text Text
    section = takeWhile1P (Just "Metadata char") (\c -> isAlphaNum c || c == '-')

chunksNE :: Parsec Void Text (NonEmpty VChunk)
chunksNE = chunkWith unit'  `PC.sepBy1` char '.'

vchunks :: Parsec Void Text [VChunk]
vchunks = chunkWith unit `sepBy` char '.'

-- | Handling @0@ is a bit tricky. We can't allow runs of zeros in a chunk,
-- since a version like @1.000.1@ would parse as @1.0.1@.
chunkWith :: Parsec Void Text VUnit -> Parsec Void Text VChunk
chunkWith u = try zeroWithLetters <|> oneZero <|> PC.some u
  where
    oneZero :: Parsec Void Text (NonEmpty VUnit)
    oneZero = (Digits 0 :| []) <$ single '0'

    zeroWithLetters :: Parsec Void Text (NonEmpty VUnit)
    zeroWithLetters = do
      z <- Digits 0 <$ single '0'
      s <- PC.some sunit
      c <- optional (chunkWith u)
      case c of
        Nothing -> pure $ NEL.cons z s
        Just c' -> pure $ NEL.cons z s <> c'

unit :: Parsec Void Text VUnit
unit = iunit <|> sunit

unit' :: Parsec Void Text VUnit
unit' = iunit <|> sunit'

iunit :: Parsec Void Text VUnit
iunit = Digits <$> ((0 <$ single '0') <|> (read <$> some digitChar))

sunit :: Parsec Void Text VUnit
sunit = Str . T.pack <$> some (letterChar <|> single '-')

-- | Same as `sunit`, but don't allow hyphens. Intended for the main body of
-- `Version`.
sunit' :: Parsec Void Text VUnit
sunit' = Str . T.pack <$> some letterChar

-- | Parse a (Haskell) `PVP`, as defined above.
pvp :: Text -> Either ParsingError PVP
pvp = parse (pvp' <* eof) "PVP"

-- | Internal megaparsec parser of `pvp`.
pvp' :: Parsec Void Text PVP
pvp' = L.lexeme space (PVP . NEL.fromList <$> L.decimal `sepBy` char '.')

-- | Parse a (General) `Version`, as defined above.
version :: Text -> Either ParsingError Version
version = parse (version'' <* eof) "Version"

-- | Internal megaparsec parser of `version`.
version' :: Parsec Void Text Version
version' = L.lexeme space version''

version'' :: Parsec Void Text Version
version'' = Version <$> optional (try epochP) <*> chunksP <*> optional releaseP <*> optional metaData

epochP :: Parsec Void Text Word
epochP = read <$> (some digitChar <* char ':')

chunksP :: Parsec Void Text Chunks
chunksP = Chunks <$> chunkWithoutHyphensP `PC.sepBy1` char '.'

-- | Parse a (Complex) `Mess`, as defined above.
mess :: Text -> Either ParsingError Mess
mess = parse (mess'' <* eof) "Mess"

-- | Internal megaparsec parser of `mess`.
mess' :: Parsec Void Text Mess
mess' = L.lexeme space mess''

mess'' :: Parsec Void Text Mess
mess'' = Mess <$> mchunks <*> optional ((,) <$> sep <*> mess')

mchunks :: Parsec Void Text (NonEmpty MChunk)
mchunks = mchunk `PC.sepBy1` char '.'

mchunk :: Parsec Void Text MChunk
mchunk = choice [ try $ (\(t, i) -> MDigit i t) <$> match (L.decimal <* next)
                , try $ (\(t, i) -> MRev i t) <$> match (single 'r' *> L.decimal <* next)
                , MPlain . T.pack <$> some (letterChar <|> digitChar) ]
  where
    next :: Parsec Void Text ()
    next = lookAhead (void (single '.') <|> void sep <|> eof)

sep :: Parsec Void Text VSep
sep = choice [ VColon  <$ char ':'
             , VHyphen <$ char '-'
             , VPlus   <$ char '+'
             , VUnder  <$ char '_'
             , VTilde  <$ char '~' ]

sepCh :: VSep -> Char
sepCh VColon  = ':'
sepCh VHyphen = '-'
sepCh VPlus   = '+'
sepCh VUnder  = '_'
sepCh VTilde  = '~'

-- | Convert any parsed Versioning type to its textual representation.
prettyV :: Versioning -> Text
prettyV (Ideal sv)  = prettySemVer sv
prettyV (General v) = prettyVer v
prettyV (Complex m) = prettyMess m

-- | Convert a `SemVer` back to its textual representation.
prettySemVer :: SemVer -> Text
prettySemVer (SemVer ma mi pa pr me) = mconcat $ ver <> pr' <> me'
  where
    ver = intersperse "." [ showt ma, showt mi, showt pa ]
    pr' = maybe [] (\m -> ["-", prettyRelease m]) pr
    me' = maybe [] (\m -> ["+", m]) me

-- | Convert a `PVP` back to its textual representation.
prettyPVP :: PVP -> Text
prettyPVP (PVP (m :| rs)) = T.intercalate "." . map showt $ m : rs

-- | Convert a `Version` back to its textual representation.
prettyVer :: Version -> Text
prettyVer (Version ep cs pr me) = mconcat $ ep' <> [ver] <> pr' <> me'
  where
    ver = prettyChunks cs
    me' = maybe [] (\m -> ["+", m]) me
    pr' = maybe [] (\m -> ["-", prettyRelease m]) pr
    ep' = maybe [] (\e -> [showt e, ":"]) ep

-- | Convert a `Mess` back to its textual representation.
prettyMess :: Mess -> Text
prettyMess (Mess t m) = case m of
  Nothing     -> t'
  Just (s, v) -> T.snoc t' (sepCh s) <> prettyMess v
  where
    t' :: Text
    t' = fold . NEL.intersperse "." $ NEL.map mchunkText t

prettyChunks :: Chunks -> Text
prettyChunks = undefined

prettyRelease :: Release -> Text
prettyRelease = undefined

chunksAsT :: Functor t => t VChunk -> t Text
chunksAsT = fmap (foldMap f)
  where
    f :: VUnit -> Text
    f (Digits i) = showt i
    f (Str s)    = s

chunksAsM :: Functor t => t VChunk -> t MChunk
chunksAsM = fmap f
  where
    f :: VChunk -> MChunk
    f (Digits i :| [])        = MDigit i $ showt i
    f (Str "r" :| [Digits i]) = MRev i . T.cons 'r' $ showt i
    f vc                      = MPlain . T.concat $ chunksAsT [vc]

--------------------------------------------------------------------------------
-- Utilities

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
showt = T.pack . show

hush :: Either a b -> Maybe b
hush (Left _)  = Nothing
hush (Right b) = Just b

data These a b = This a | That b | Both a b

zipLongest :: [a] -> [b] -> [These a b]
zipLongest [] []         = []
zipLongest (a:as) (b:bs) = Both a b : zipLongest as bs
zipLongest (a:as) []     = This a : zipLongest as []
zipLongest [] (b:bs)     = That b : zipLongest [] bs
