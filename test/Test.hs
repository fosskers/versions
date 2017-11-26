{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Either (isRight, isLeft)
import Data.List (groupBy)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Versions
import Lens.Micro
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

---

instance Arbitrary SemVer where
  arbitrary = SemVer <$> arbitrary <*> arbitrary <*> arbitrary <*> chunks <*> chunks

-- | Sane generatin of VChunks.
chunks :: Gen [[VUnit]]
chunks = resize 10 . listOf1 . fmap simplify . resize 10 $ listOf1 arbitrary

simplify :: [VUnit] -> [VUnit]
simplify = map (foldl1 mappend) . groupBy f
  where f (Digits _) (Digits _) = True
        f (Str _) (Str _) = True
        f _ _ = False

instance EqProp SemVer where
  a =-= b = eq a b

instance Arbitrary VUnit where
  arbitrary = frequency [ (1, Digits . (+ 1) <$> arbitrary) , (1, s) ]
    where s = Str . pack . map unletter <$> resize 10 (listOf1 arbitrary)

-- | An ASCII letter.
newtype Letter = Letter { unletter :: Char }

instance Arbitrary Letter where
  arbitrary = Letter . chr <$> choose (97, 122)

instance Arbitrary Version where
  arbitrary = Version <$> arbitrary <*> chunks <*> chunks

instance EqProp Version where
  a =-= b = eq a b

-- instance CoArbitrary VUnit

-- | These don't need to parse as a SemVer.
goodVers :: [Text]
goodVers = [ "1", "1.2", "1.0rc0", "1.0rc1", "1.1rc1", "1.58.0-3",  "44.0.2403.157-1"
           , "0.25-2",  "8.u51-1", "21-2", "7.1p1-1", "20150826-1", "1:0.10.16-3"
           ]

messes :: [Text]
messes = [ "10.2+0.93+1-1", "003.03-3", "002.000-7", "20.26.1_0-2" ]

messComps :: [Text]
messComps = [ "10.2+0.93+1-1", "10.2+0.93+1-2", "10.2+0.93+2-1"
            , "10.2+0.94+1-1", "10.3+0.93+1-1", "11.2+0.93+1-1", "12"
            ]

badSemVs :: [Text]
badSemVs = [ "1", "1.2", "1.2.3+a1b2bc3.1-alpha.2", "a.b.c", "1.01.1"
           , "1.2.3+a1b!2c3.1"
           ]

goodSemVs :: [Text]
goodSemVs = [ "0.1.0", "1.2.3", "1.2.3-1", "1.2.3-alpha", "1.2.3-alpha.2"
            , "1.2.3+a1b2c3.1", "1.2.3-alpha.2+a1b2c3.1"
            ]

-- | The exact example from `http://semver.org`
semverOrd :: [Text]
semverOrd = [ "1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta"
            , "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1"
            , "1.0.0"
            ]

-- | Cabal makes this distinction: 0.2 < 0.2.0 < 0.2.0.0
-- Apparently there are only 5 packages on Hackage that actually
-- make this necessary, meaning `cabal` can't be simplified to ignore it.
-- Logically, these are the same package, but for those 5 packages, they
-- aren't.
cabalOrd :: [Text]
cabalOrd = [ "0.2", "0.2.0", "0.2.0.0" ]

versionOrd :: [Text]
versionOrd = [ "0.9.9.9", "1.0.0.0", "1.0.0.1", "2" ]

suite :: TestTree
suite = testGroup "Tests"
  [ testGroup "Property Tests"
    [ testGroup "SemVer - Monoid" $
      map (\(name, test) -> testProperty name test) . unbatch $ monoid (SemVer 1 2 3 [] [])
    , testProperty "SemVer - Arbitrary" $ \a -> isRight . fmap (== a) $ semver (prettySemVer a)
    , testProperty "Version - Arbitrary" $ \a -> isRight . fmap (== a) $ version (prettyVer a)
    -- , testGroup "Version - Monoid" $
    --   map (\(name, test) -> testProperty name test) . unbatch $ monoid (Version (Just 1) [[digits 2], [digits 3]])
    ]
  , testGroup "Unit Tests"
    [ testGroup "(Ideal) Semantic Versioning"
      [ testGroup "Bad Versions (shouldn't parse)" $
        map (\s -> testCase (unpack s) $ assert $ isLeft $ semver s) badSemVs
      , testGroup "Good Versions (should parse)" $
        map (\s -> testCase (unpack s) $ isomorphSV s) goodSemVs
      , testGroup "Comparisons" $
        testCase "1.2.3-alpha.2 == 1.2.3-alpha.2+a1b2c3.1"
        (assert $ semver "1.2.3-alpha.2" == semver "1.2.3-alpha.2+a1b2c3.1") :
        map (\(a,b) -> testCase (unpack $ a <> " < " <> b) $ comp semver a b)
        (zip semverOrd $ tail semverOrd)
      ]
    , testGroup "(General) Versions"
      [ testGroup "Good Versions" $
        map (\s -> testCase (unpack s) $ isomorphV s) goodVers
      , testGroup "Comparisons" $
        testCase "1.2-5 < 1.2.3-1" (comp version "1.2-5" "1.2.3-1") :
        testCase "1.0rc1 < 1.0" (comp version "1.0rc1" "1.0") :
        testCase "1.0 < 1:1.0" (comp version "1.0" "1:1.0") :
        testCase "1.1 < 1:1.0" (comp version "1.1" "1:1.0") :
        testCase "1.1 < 1:1.1" (comp version "1.1" "1:1.1") :
        map (\(a,b) -> testCase (unpack $ a <> " < " <> b) $ comp version a b)
        (zip cabalOrd (tail cabalOrd) <> zip versionOrd (tail versionOrd))
      ]
    , testGroup "(Complex) Mess"
      [ testGroup "Good Versions" $
        map (\s -> testCase (unpack s) $ isomorphM s) messes
      , testGroup "Comparisons" $
        map (\(a,b) -> testCase (unpack $ a <> " < " <> b) $ comp mess a b) $
        zip messComps (tail messComps)
      ]
    , testGroup "Mixed Versioning"
      [ testGroup "Identification"
        [ testCase "1.2.3 is SemVer" $ check $ isSemVer <$> parseV "1.2.3"
        , testCase "1.2.3-1 is SemVer" $ check $ isSemVer <$> parseV "1.2.3-1"
        , testCase "1.2.3-1+1 is SemVer" $ check $ isSemVer <$> parseV "1.2.3-1+1"
        , testCase "1.2.3r1 is Version" $ check $ isVersion <$> parseV "1.2.3r1"
        , testCase "0.25-2 is Version" $ check $ isVersion <$> parseV "0.25-2"
        , testCase "1:1.2.3-1 is Version" $ check $ isVersion <$> parseV "1:1.2.3-1"
        , testCase "1.2.3+1-1 is Mess" $ check $ isMess <$> parseV "1.2.3+1-1"
        , testCase "000.007-1 is Mess" $ check $ isMess <$> parseV "000.007-1"
        , testCase "20.26.1_0-2 is Mess" $ check $ isMess <$> parseV "20.26.1_0-2"
        ]
      , testGroup "Isomorphisms" $
        map (\s -> testCase (unpack s) $ isomorph s) $ goodSemVs ++ goodVers ++ messes
      , testGroup "Comparisons"
        [ testCase "1.2.2r1-1 < 1.2.3-1"   $ comp parseV "1.2.2r1-1" "1.2.3-1"
        , testCase "1.2.3-1   < 1.2.4r1-1" $ comp parseV "1.2.3-1" "1.2.4r1-1"
        , testCase "1.2.3-1   < 2+0007-1"  $ comp parseV "1.2.3-1" "2+0007-1"
        , testCase "1.2.3r1-1 < 2+0007-1"  $ comp parseV "1.2.3r1-1" "2+0007-1"
        , testCase "1.2-5 < 1.2.3-1"       $ comp parseV "1.2-5" "1.2.3-1"
        ]
      ]
    , testGroup "Lenses and Traversals"
      [ testCase "SemVer - Increment Patch" incPatch
      , testCase "SemVer - Increment Patch from Text" incFromT
      , testCase "SemVer - Get patches" patches
      , testCase "Traverse `General` as `Ideal`" noInc
      ]
    ]
  ]

-- | Does pretty-printing return a Versioning to its original form?
isomorph :: Text -> Assertion
isomorph t = Right t @=? (prettyV <$> parseV t)

-- | Does pretty-printing return a Version to its original form?
isomorphV :: Text -> Assertion
isomorphV t = Right t @=? (prettyVer <$> version t)

-- | Does pretty-printing return a SemVer to its original form?
isomorphSV :: Text -> Assertion
isomorphSV t = Right t @=? (prettySemVer <$> semver t)

isomorphM :: Text -> Assertion
isomorphM t =  Right t @=? (prettyMess <$> mess t)

comp :: Ord b => (Text -> Either a b) -> Text -> Text -> Assertion
comp f a b = check $ (<) <$> f a <*> f b

check :: Either a Bool -> Assertion
check = assert . either (const False) id

isSemVer :: Versioning -> Bool
isSemVer (Ideal _) = True
isSemVer _ = False

isVersion :: Versioning -> Bool
isVersion (General _) = True
isVersion _ = False

isMess :: Versioning -> Bool
isMess (Complex _) = True
isMess _ = False

incPatch :: Assertion
incPatch = (v1 & patch %~ (+ 1)) @?= v2
  where v1 = Ideal $ SemVer 1 2 3 [] []
        v2 = Ideal $ SemVer 1 2 4 [] []

-- | Nothing should happen.
noInc :: Assertion
noInc = (v & patch %~ (+ 1)) @?= v
  where v = General $ Version Nothing [] []

incFromT :: Assertion
incFromT = (("1.2.3" :: Text) & patch %~ (+ 1)) @?= "1.2.4"

patches :: Assertion
patches = ps @?= [3,4,5]
  where ps = (["1.2.3","2.3.4","3.4.5"] :: [Text]) ^.. each . patch

main :: IO ()
main = defaultMain suite
