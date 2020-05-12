{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}

-- |
-- Module    : Data.Versions
-- Copyright : (c) Colin Woodbury, 2015 - 2020
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
-- == Using the Parsers
-- In general, `versioning` is the function you want. It attempts to parse a
-- given `T.Text` using the three individual parsers, `semver`, `version` and
-- `mess`. If one fails, it tries the next. If you know you only want to parse
-- one specific version type, use that parser directly (e.g. `semver`).

module Data.Versions
  ( -- * Types
    Versioning(..)
  , isIdeal, isGeneral, isComplex
  , SemVer(..)
  , PVP(..)
  , Version(..)
  , Mess(..)
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
    -- | When traversing `T.Text`, leveraging its `Semantic` instance will
    -- likely benefit you more than using these Traversals directly.
  , _Versioning, _SemVer, _Version, _Mess
    -- ** Versioning Traversals
  , _Ideal, _General, _Complex
    -- ** (General) Version Lenses
  , epoch
    -- ** Misc. Lenses / Traversals
  , _Digits, _Str
  ) where

import           Control.DeepSeq
import           Data.Bool (bool)
import           Data.Char (isAlpha)
import           Data.Hashable (Hashable)
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           Text.Megaparsec hiding (chunk)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

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
_Versioning :: Traversal' T.Text Versioning
_Versioning f t = either (const (pure t)) (fmap prettyV . f) $ versioning t
{-# INLINE _Versioning #-}

-- | Traverse some Text for its inner SemVer.
_SemVer :: Traversal' T.Text SemVer
_SemVer f t = either (const (pure t)) (fmap prettySemVer . f) $ semver t
{-# INLINE _SemVer #-}

-- | Traverse some Text for its inner Version.
_Version :: Traversal' T.Text Version
_Version f t = either (const (pure t)) (fmap prettyVer . f) $ version t
{-# INLINE _Version #-}

-- | Traverse some Text for its inner Mess.
_Mess :: Traversal' T.Text Mess
_Mess f t = either (const (pure t)) (fmap prettyMess . f) $ mess t
{-# INLINE _Mess #-}

_Ideal :: Traversal' Versioning SemVer
_Ideal f (Ideal s) = Ideal <$> f s
_Ideal _ v         = pure v
{-# INLINE _Ideal #-}

_General :: Traversal' Versioning Version
_General f (General v) = General <$> f v
_General _ v           = pure v
{-# INLINE _General #-}

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
  release  :: Traversal' v [VChunk]
  -- | @major.minor.patch-prerel+META@
  meta     :: Traversal' v [VChunk]
  -- | A Natural Transformation into an proper `SemVer`.
  semantic :: Traversal' v SemVer

instance Semantic T.Text where
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
-- 3. PREREL and META strings may only contain ASCII alphanumerics.
--
-- For more information, see http://semver.org
data SemVer = SemVer
  { _svMajor  :: Word
  , _svMinor  :: Word
  , _svPatch  :: Word
  , _svPreRel :: [VChunk]
  , _svMeta   :: [VChunk] }
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
     EQ -> case (pr,pr') of
            ([],[]) -> EQ
            ([],_)  -> GT
            (_,[])  -> LT
            _       -> compare pr pr'

instance Semigroup SemVer where
  SemVer mj mn pa p m <> SemVer mj' mn' pa' p' m' =
    SemVer (mj + mj') (mn + mn') (pa + pa') (p ++ p') (m ++ m')

instance Monoid SemVer where
  mempty = SemVer 0 0 0 [] []

#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

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

-- | A single unit of a Version. May be digits or a string of characters. Groups
-- of these are called `VChunk`s, and are the identifiers separated by periods
-- in the source.
data VUnit = Digits Word | Str T.Text
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Semigroup VUnit where
  Digits n <> Digits m = Digits $ n + m
  Str t    <> Str s    = Str $ t <> s
  Digits n <> _        = Digits n
  _        <> Digits n = Digits n

instance Monoid VUnit where
  mempty = Str ""

#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

-- | Smart constructor for a `VUnit` made of digits.
digits :: Word -> VUnit
digits = Digits

-- | Smart constructor for a `VUnit` made of letters.
str :: T.Text -> Maybe VUnit
str t = bool Nothing (Just $ Str t) $ T.all isAlpha t

_Digits :: Traversal' VUnit Word
_Digits f (Digits i) = Digits <$> f i
_Digits _ v          = pure v
{-# INLINE _Digits #-}

_Str :: Traversal' VUnit T.Text
_Str f (Str t) = Str . (\t' -> bool t t' (T.all isAlpha t')) <$> f t
_Str _ v       = pure v
{-# INLINE _Str #-}

-- | A logical unit of a version number. Can consist of multiple letters
-- and numbers.
type VChunk = [VUnit]

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

instance Semigroup PVP where
  PVP (m :| r) <> PVP (m' :| r') = PVP $ (m + m') :| f r r'
    where
      f a []          = a
      f [] b          = b
      f (a:as) (b:bs) = (a + b) : f as bs

instance Monoid PVP where
  mempty = PVP (0 :| [])

#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

instance Semantic PVP where
  major f (PVP (m :| rs)) = (\ma -> PVP $ ma :| rs) <$> f m
  {-# INLINE major #-}

  minor f (PVP (m :| mi : rs)) = (\mi' -> PVP $ m :| mi' : rs) <$> f mi
  minor f (PVP (m :| []))      = (\mi' -> PVP $ m :| [mi']) <$> f 0
  {-# INLINE minor #-}

  patch f (PVP (m :| mi : pa : rs)) = (\pa' -> PVP $ m :| mi : pa' : rs) <$> f pa
  patch f (PVP (m :| mi : []))      = (\pa' -> PVP $ m :| mi : [pa']) <$> f 0
  patch f (PVP (m :| []))           = (\pa' -> PVP $ m :| 0 : [pa']) <$> f 0
  {-# INLINE patch #-}

  release f p = const p <$> f []
  {-# INLINE release #-}

  meta f p = const p <$> f []
  {-# INLINE meta #-}

  semantic f (PVP (m :| rs)) = (\(SemVer ma mi pa _ _) -> PVP $ ma :| [mi, pa]) <$> f s
    where
      s = case rs of
        mi : pa : _ -> SemVer m mi pa [] []
        mi : _      -> SemVer m mi 0  [] []
        []          -> SemVer m 0 0   [] []
  {-# INLINE semantic #-}

--------------------------------------------------------------------------------
-- (General) Version

-- | A (General) Version.
-- Not quite as ideal as a `SemVer`, but has some internal consistancy
-- from version to version.
-- Generally conforms to the @x.x.x-x@ pattern, and may optionally have an /epoch/.
-- These are prefixes marked by a colon, like in @1:2.3.4@.
--
-- Examples of @Version@ that are not @SemVer@: 0.25-2, 8.u51-1, 20150826-1, 1:2.3.4
data Version = Version
  { _vEpoch  :: Maybe Word
  , _vChunks :: [VChunk]
  , _vRel    :: [VChunk] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance Semigroup Version where
  Version e c r <> Version e' c' r' = Version ((+) <$> e <*> e') (c ++ c') (r ++ r')

instance Monoid Version where
  mempty = Version Nothing [] []

#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

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

  -- | If a version has an epoch > 1 and the other has no epoch, the first will
  -- be considered greater.
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
  compare Version{}  (Version _ [] _) = GT
  compare (Version _ [] _) Version{}  = LT

  -- | The usual case. If first VChunks of each Version is equal, then we keep
  -- recursing. Otherwise, we don't need to check further. Consider @1.2@
  -- compared to @1.1.3.4.5.6@.
  compare (Version _ (a:as) rs) (Version _ (b:bs) rs') = case f a b of
    EQ  -> compare (Version Nothing as rs) (Version Nothing bs rs')
    res -> res
    where f [] [] = EQ

          -- | Opposite of the above. If we've recursed this far and one side
          -- has fewer chunks, it must be the "greater" version. A Chunk break
          -- only occurs in a switch from digits to letters and vice versa, so
          -- anything "extra" must be an @rc@ marking or similar. Consider @1.1@
          -- compared to @1.1rc1@.
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

instance Semantic Version where
  major f (Version e ([Digits n] : cs) rs) = (\n' -> Version e ([Digits n'] : cs) rs) <$> f n
  major _ v = pure v
  {-# INLINE major #-}

  minor f (Version e (c : [Digits n] : cs) rs) = (\n' -> Version e (c : [Digits n'] : cs) rs) <$> f n
  minor _ v = pure v
  {-# INLINE minor #-}

  patch f (Version e (c : d : [Digits n] : cs) rs) = (\n' -> Version e (c : d : [Digits n'] : cs) rs) <$> f n
  patch _ v = pure v
  {-# INLINE patch #-}

  -- | This will always succeed.
  release f v = fmap (\vr -> v { _vRel = vr }) (f $ _vRel v)
  {-# INLINE release #-}

  -- | This will always fail.
  meta _ v = pure v
  {-# INLINE meta #-}

  semantic f (Version _ ([Digits a] : [Digits b] : [Digits c] : _) rs) = vFromS <$> f (SemVer a b c rs [])
  semantic _ v = pure v
  {-# INLINE semantic #-}

epoch :: Lens' Version (Maybe Word)
epoch f v = fmap (\ve -> v { _vEpoch = ve }) (f $ _vEpoch v)
{-# INLINE epoch #-}

--------------------------------------------------------------------------------
-- (Complex) Mess

-- | A (Complex) Mess. This is a /descriptive/ parser, based on examples of
-- stupidly crafted version numbers used in the wild.
--
-- Groups of letters/numbers, separated by a period, can be further separated by
-- the symbols @_-+:@
--
-- Unfortunately, @VChunk@s cannot be used here, as some developers have numbers
-- like @1.003.04@ which make parsers quite sad.
--
-- Not guaranteed to have well-defined ordering (@Ord@) behaviour, but so far
-- internal tests show consistency.
data Mess = VLeaf [T.Text] | VNode [T.Text] VSep Mess
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance Ord Mess where
  compare (VLeaf l1) (VLeaf l2)     = compare l1 l2
  compare (VNode t1 _ _) (VLeaf t2) = compare t1 t2
  compare (VLeaf t1) (VNode t2 _ _) = compare t1 t2
  compare (VNode t1 _ v1) (VNode t2 _ v2) | t1 < t2 = LT
                                          | t1 > t2 = GT
                                          | otherwise = compare v1 v2

instance Semantic Mess where
  major f v@(VNode (t : ts) s ms) = either (const $ pure v) g $ parse digitsP "Major" t
    where g n = (\n' -> VNode (showt n' : ts) s ms) <$> f n
  major _ v = pure v
  {-# INLINE major #-}

  minor f v@(VNode (t0 : t : ts) s ms) = either (const $ pure v) g $ parse digitsP "Minor" t
    where g n = (\n' -> VNode (t0 : showt n' : ts) s ms) <$> f n
  minor _ v = pure v
  {-# INLINE minor #-}

  patch f v@(VNode (t0 : t1 : t : ts) s ms) = either (const $ pure v) g $ parse digitsP "Patch" t
    where g n = (\n' -> VNode (t0 : t1 : showt n' : ts) s ms) <$> f n
  patch _ v = pure v
  {-# INLINE patch #-}

  -- | This will always fail.
  release _ v = pure v
  {-# INLINE release #-}

  -- | This will always fail.
  meta _ v = pure v
  {-# INLINE meta #-}

  -- | Good luck.
  semantic f v@(VNode (t0 : t1 : t2 : _) _ _) = either (const $ pure v) (fmap (mFromV . vFromS)) $
    (\a b c -> f $ SemVer a b c [] [])
    <$> parse digitsP "Major" t0
    <*> parse digitsP "Minor" t1
    <*> parse digitsP "Patch" t2
  semantic _ v = pure v
  {-# INLINE semantic #-}

-- | Developers use a number of symbols to seperate groups of digits/letters in
-- their version numbers. These are:
--
-- * A colon (:). Often denotes an "epoch".
-- * A hyphen (-).
-- * A plus (+). Stop using this outside of metadata if you are. Example: @10.2+0.93+1-1@
-- * An underscore (_). Stop using this if you are.
data VSep = VColon | VHyphen | VPlus | VUnder
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

--------------------------------------------------------------------------------
-- Parsing

-- | A synonym for the more verbose `megaparsec` error type.
type ParsingError = ParseErrorBundle T.Text Void

-- | Parse a piece of `T.Text` into either an (Ideal) `SemVer`, a (General)
-- `Version`, or a (Complex) `Mess`.
versioning :: T.Text -> Either ParsingError Versioning
versioning = parse versioning' "versioning"

-- | Parse a `Versioning`. Assumes the version number is the last token in
-- the string.
versioning' :: Parsec Void T.Text Versioning
versioning' = choice [ try (fmap Ideal semver'    <* eof)
                     , try (fmap General version' <* eof)
                     , fmap Complex mess'         <* eof ]

-- | Parse a (Ideal) Semantic Version.
semver :: T.Text -> Either ParsingError SemVer
semver = parse (semver' <* eof) "Semantic Version"

-- | Internal megaparsec parser of `semver`.
semver' :: Parsec Void T.Text SemVer
semver' = L.lexeme space (SemVer <$> majorP <*> minorP <*> patchP <*> preRel <*> metaData)

-- | Parse a group of digits, which can't be lead by a 0, unless it is 0.
digitsP :: Parsec Void T.Text Word
digitsP = read <$> ((T.unpack <$> string "0") <|> some digitChar)

majorP :: Parsec Void T.Text Word
majorP = digitsP <* char '.'

minorP :: Parsec Void T.Text Word
minorP = majorP

patchP :: Parsec Void T.Text Word
patchP = digitsP

preRel :: Parsec Void T.Text [VChunk]
preRel = (char '-' *> chunks) <|> pure []

metaData :: Parsec Void T.Text [VChunk]
metaData = (char '+' *> chunks) <|> pure []

chunks :: Parsec Void T.Text [VChunk]
chunks = chunk `sepBy` char '.'

-- | Handling @0@ is a bit tricky. We can't allow runs of zeros in a chunk,
-- since a version like @1.000.1@ would parse as @1.0.1@.
chunk :: Parsec Void T.Text VChunk
chunk = try zeroWithLetters <|> oneZero <|> many (iunit <|> sunit)
  where oneZero = (:[]) . Digits . read . T.unpack <$> string "0"
        zeroWithLetters = do
          z <- Digits . read . T.unpack <$> string "0"
          s <- some sunit
          c <- chunk
          pure $ (z : s) ++ c

iunit :: Parsec Void T.Text VUnit
iunit = Digits . read <$> some digitChar

sunit :: Parsec Void T.Text VUnit
sunit = Str . T.pack <$> some letterChar

-- | Parse a (Haskell) `PVP`, as defined above.
pvp :: T.Text -> Either ParsingError PVP
pvp = parse (pvp' <* eof) "PVP"

-- | Internal megaparsec parser of `pvp`.
pvp' :: Parsec Void T.Text PVP
pvp' = L.lexeme space (PVP . NEL.fromList <$> L.decimal `sepBy` char '.')

-- | Parse a (General) `Version`, as defined above.
version :: T.Text -> Either ParsingError Version
version = parse (version' <* eof) "Version"

-- | Internal megaparsec parser of `version`.
version' :: Parsec Void T.Text Version
version' = L.lexeme space (Version <$> optional (try epochP) <*> chunks <*> preRel)

epochP :: Parsec Void T.Text Word
epochP = read <$> (some digitChar <* char ':')

-- | Parse a (Complex) `Mess`, as defined above.
mess :: T.Text -> Either ParsingError Mess
mess = parse (mess' <* eof) "Mess"

-- | Internal megaparsec parser of `mess`.
mess' :: Parsec Void T.Text Mess
mess' = L.lexeme space (try node <|> leaf)

leaf :: Parsec Void T.Text Mess
leaf = VLeaf <$> tchunks

node :: Parsec Void T.Text Mess
node = VNode <$> tchunks <*> sep <*> mess'

tchunks :: Parsec Void T.Text [T.Text]
tchunks = (T.pack <$> some (letterChar <|> digitChar)) `sepBy` char '.'

sep :: Parsec Void T.Text VSep
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
prettyV :: Versioning -> T.Text
prettyV (Ideal sv)  = prettySemVer sv
prettyV (General v) = prettyVer v
prettyV (Complex m) = prettyMess m

-- | Convert a `SemVer` back to its textual representation.
prettySemVer :: SemVer -> T.Text
prettySemVer (SemVer ma mi pa pr me) = mconcat $ ver <> pr' <> me'
  where ver = intersperse "." [ showt ma, showt mi, showt pa ]
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)
        me' = foldable [] ("+" :) $ intersperse "." (chunksAsT me)

-- | Convert a `PVP` back to its textual representation.
prettyPVP :: PVP -> T.Text
prettyPVP (PVP (m :| rs)) = T.intercalate "." . map showt $ m : rs

-- | Convert a `Version` back to its textual representation.
prettyVer :: Version -> T.Text
prettyVer (Version ep cs pr) = ep' <> mconcat (ver <> pr')
  where ver = intersperse "." $ chunksAsT cs
        pr' = foldable [] ("-" :) $ intersperse "." (chunksAsT pr)
        ep' = maybe "" (\e -> showt e <> ":") ep

-- | Convert a `Mess` back to its textual representation.
prettyMess :: Mess -> T.Text
prettyMess (VLeaf t)     = mconcat $ intersperse "." t
prettyMess (VNode t s v) = T.snoc t' (sepCh s) <> prettyMess v
  where t' = mconcat $ intersperse "." t

chunksAsT :: [VChunk] -> [T.Text]
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
showt :: Show a => a -> T.Text
showt = T.pack . show
