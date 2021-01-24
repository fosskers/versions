{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main ( main ) where

import           Data.Char (chr)
import           Data.Either (isLeft)
import           Data.Foldable (fold)
import           Data.List (groupBy)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import           Data.Versions
import           Data.Void (Void)
import           Lens.Micro
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Printf (printf)

---

instance Arbitrary SemVer where
  arbitrary = SemVer <$> arbitrary <*> arbitrary <*> arbitrary <*> chunks <*> chunks

-- | Sane generation of VChunks.
chunks :: Gen [VChunk]
chunks = resize 10 . listOf1 . fmap simplify . resize 10 $ listOf1 arbitrary

chunksNE :: Gen (NEL.NonEmpty VChunk)
chunksNE = NEL.fromList <$> chunks

simplify :: [VUnit] -> NEL.NonEmpty VUnit
simplify = NEL.fromList . map fold . groupBy f
  where f (Digits _) (Digits _) = True
        f (Str _) (Str _)       = True
        f _ _                   = False

instance Arbitrary VUnit where
  arbitrary = frequency [ (1, Digits . (+ 1) <$> arbitrary) , (1, s) ]
    where s = Str . T.pack . map unletter <$> resize 10 (listOf1 arbitrary)

-- | An ASCII letter.
newtype Letter = Letter { unletter :: Char }

instance Arbitrary Letter where
  arbitrary = Letter . chr <$> choose (97, 122)

instance Arbitrary Version where
  arbitrary = Version <$> arbitrary <*> chunksNE <*> chunks <*> chunks

-- | These don't need to parse as a SemVer.
goodVers :: [T.Text]
goodVers = [ "1", "1.2", "1.0rc0", "1.0rc1", "1.1rc1", "1.58.0-3",  "44.0.2403.157-1"
           , "0.25-2",  "8.u51-1", "21-2", "7.1p1-1", "20150826-1", "1:0.10.16-3"
           , "1.11.0.git.20200404-1", "1.11.0+20200830-1", "1:3.20" ]

badVers :: [T.Text]
badVers = ["", "1.2 "]

messes :: [T.Text]
messes = [ "10.2+0.93+1-1", "003.03-3", "002.000-7", "20.26.1_0-2", "1.6.0a+2014+m872b87e73dfb-1"
         , "1.3.00.16851-1", "5.2.458699.0906-1" ]

messComps :: [T.Text]
messComps = [ "10.2+0.93+1-1", "10.2+0.93+1-2", "10.2+0.93+2-1"
            , "10.2+0.94+1-1", "10.3+0.93+1-1", "11.2+0.93+1-1", "12"
            ]

badSemVs :: [T.Text]
badSemVs = [ "1", "1.2", "1.2.3+a1b2bc3.1-alpha.2", "a.b.c", "1.01.1"
           , "1.2.3+a1b!2c3.1", "", "1.2.3 "
           ]

goodSemVs :: [T.Text]
goodSemVs = [ "0.1.0", "1.2.3", "1.2.3-1", "1.2.3-alpha", "1.2.3-alpha.2"
            , "1.2.3+a1b2c3.1", "1.2.3-alpha.2+a1b2c3.1", "2.2.1-b05"
            ]

-- | The exact example from `http://semver.org`
semverOrd :: [T.Text]
semverOrd = [ "1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta"
            , "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1"
            , "1.0.0"
            ]

-- | Cabal makes this distinction: 0.2 < 0.2.0 < 0.2.0.0
-- Apparently there are only 5 packages on Hackage that actually
-- make this necessary, meaning `cabal` can't be simplified to ignore it.
-- Logically, these are the same package, but for those 5 packages, they
-- aren't.
cabalOrd :: [T.Text]
cabalOrd = [ "0", "0.2", "0.2.0", "0.2.0.0" ]

versionOrd :: [T.Text]
versionOrd = [ "0.9.9.9", "1.0.0.0", "1.0.0.1", "2" ]

suite :: TestTree
suite = testGroup "Tests"
  [ testGroup "Property Tests"
    [ testProperty "SemVer - Arbitrary" $ \a -> semver (prettySemVer a) == Right a
    , testProperty "Version - Arbitrary" $ \a -> version (prettyVer a) == Right a
    -- , testGroup "Version - Monoid" $
    --   map (\(name, test) -> testProperty name test) . unbatch $ monoid (Version (Just 1) [[digits 2], [digits 3]])
    ]
  , testGroup "Unit Tests"
    [ testGroup "(Ideal) Semantic Versioning"
      [ testGroup "Bad Versions (shouldn't parse)" $
        map (\s -> testCase (T.unpack s) $ assertBool "A bad version parsed" $ isLeft $ semver s) badSemVs
      , testGroup "Good Versions (should parse)" $
        map (\s -> testCase (T.unpack s) $ isomorphSV s) goodSemVs
      , testGroup "Comparisons" $
        testCase "1.2.3-alpha.2 == 1.2.3-alpha.2+a1b2c3.1"
        (assertBool "Equality test of two complicated SemVers failed"
         $ semver "1.2.3-alpha.2" == semver "1.2.3-alpha.2+a1b2c3.1") :
        map (\(a,b) -> testCase (T.unpack $ a <> " < " <> b) $ comp semver a b)
        (zip semverOrd $ tail semverOrd)
      , testGroup "Whitespace Handling"
        [ testCase "1.2.3-1[ ]" $ parse semver' "semver whitespace" "1.2.3-1 " @?= Right (SemVer 1 2 3 [[Digits 1]] [])
        ]
      , testGroup "Zero Handling"
        [ testCase "2.2.1-b05" $ semver "2.2.1-b05" @?= Right (SemVer 2 2 1 [[Str "b", Digits 0, Digits 5]] [])

        ]
      ]
    , testGroup "(Haskell) PVP"
      [ testGroup "Good PVPs" $
        map (\s -> testCase (T.unpack s) $ isomorphPVP s) cabalOrd
      , testGroup "Comparisons" $
        map (\(a,b) -> testCase (T.unpack $ a <> " < " <> b) $ comp pvp a b)
        (zip cabalOrd $ tail cabalOrd)
      ]
    , testGroup "(General) Versions"
      [ testGroup "Good Versions" $
        map (\s -> testCase (T.unpack s) $ isomorphV s) goodVers
      , testGroup "Bad Versions (shouldn't parse)" $
        map (\s -> testCase (T.unpack s) $ assertBool "A bad version parsed" $ isLeft $ version s) badVers
      , testGroup "Comparisons" $
        testCase "1.2-5 < 1.2.3-1" (comp version "1.2-5" "1.2.3-1") :
        testCase "1.0rc1 < 1.0" (comp version "1.0rc1" "1.0") :
        testCase "1.0 < 1:1.0" (comp version "1.0" "1:1.0") :
        testCase "1.1 < 1:1.0" (comp version "1.1" "1:1.0") :
        testCase "1.1 < 1:1.1" (comp version "1.1" "1:1.1") :
        map (\(a,b) -> testCase (T.unpack $ a <> " < " <> b) $ comp version a b)
        (zip cabalOrd (tail cabalOrd) <> zip versionOrd (tail versionOrd))
      ]
    , testGroup "(Complex) Mess"
      [ testGroup "Good Versions" $
        map (\s -> testCase (T.unpack s) $ isomorphM s) messes
      , testGroup "Bad Versions (shouldn't parse)" $
        map (\s -> testCase (T.unpack s) $ assertBool "A bad version parsed" $ isLeft $ mess s) badVers
      , testGroup "Comparisons" $
        map (\(a,b) -> testCase (T.unpack $ a <> " < " <> b) $ comp mess a b) $
        zip messComps (tail messComps)
      , testGroup "SemVer-like Value Extraction"
        [ testCase "messMajor" $
          (hush (mess "1.6.0a+2014+m872b87e73dfb-1") >>= messMajor) @?= Just 1
        , testCase "messMinor" $
          (hush (mess "1.6.0a+2014+m872b87e73dfb-1") >>= messMinor) @?= Just 6
        , testCase "messPatch - Good" $
          (hush (mess "1.6.0+2014+m872b87e73dfb-1") >>= messPatch) @?= Just 0
        , testCase "messPatch - Bad" $
          (hush (mess "1.6.0a+2014+m872b87e73dfb-1") >>= messPatch) @?= Nothing
        , testCase "messPatchChunk" $
          (hush (mess "1.6.0a+2014+m872b87e73dfb-1") >>= messPatchChunk) @?= Just [Digits 0, Str "a"]
        ]
      ]
    , testGroup "Mixed Versioning"
      [ testGroup "Identification"
        [ testCase "1.2.3 is SemVer" $ check $ isSemVer <$> versioning "1.2.3"
        , testCase "1.2.3-1 is SemVer" $ check $ isSemVer <$> versioning "1.2.3-1"
        , testCase "1.2.3-1+1 is SemVer" $ check $ isSemVer <$> versioning "1.2.3-1+1"
        , testCase "1.2.3r1 is Version" $ check $ isVersion <$> versioning "1.2.3r1"
        , testCase "0.25-2 is Version" $ check $ isVersion <$> versioning "0.25-2"
        , testCase "1:1.2.3-1 is Version" $ check $ isVersion <$> versioning "1:1.2.3-1"
        , testCase "1.2.3+1-1 is Version" $ check $ isVersion <$> versioning "1.2.3+1-1"
        , testCase "1:3.20.1-1 is Version" $ check $ isVersion <$> versioning "1:3.20.1-1"
        , testCase "000.007-1 is Mess" $ check $ isMess <$> versioning "000.007-1"
        , testCase "20.26.1_0-2 is Mess" $ check $ isMess <$> versioning "20.26.1_0-2"
        , testCase "1:3.20.1-1 is Version" $ check $ isVersion <$> versioning "1:3.20.1-1"
        ]
      , testGroup "Bad Versions" $
        map (\s -> testCase (T.unpack s) $ assertBool "A bad version parsed" $ isLeft $ versioning s) badVers
      , testGroup "Isomorphisms" $
        map (\s -> testCase (T.unpack s) $ isomorph s) $ goodSemVs ++ goodVers ++ messes
      , testGroup "Comparisons"
        [ compVer "1.2.2r1-1" "1.2.3-1"
        , compVer "1.2.3-1" "1.2.4r1-1"
        , compVer "1.2.3-1" "2+0007-1"
        , compVer "1.2.3r1-1" "2+0007-1"
        , compVer "1.2-5" "1.2.3-1"
        , compVer "1.6.0a+2014+m872b87e73dfb-1" "1.6.0-1"
        , compVer "1.11.0.git.20200404-1" "1.11.0+20200830-1"
        , compVer "0.17.0+r8+gc41db5f1-1" "0.17.0+r157+g584760cf-1"
        , compVer "0.4.8-1" "0.4.9-1"
        , compVer "7.42.13-4" "7.46.0-2"
        , compVer "1.15.2-1" "1.15.3-1"
        , compVer "2.1.16102-2" "2.1.17627-1"
        , compVer "8.64.0.81-1" "8.65.0.78-1"
        , compVer "1.3.00.16851-1" "1.3.00.25560-1"
        , compVer "10.0.4-1" "10.1.0-1"
        , compVer "1:3.20-1" "1:3.20.1-1"
        , compVer "5.2.458699.0906-1" "5.3.472687.1012-1"
        ]
      , testGroup "Equality"
        [ eqVer "1:3.20.1-1"
        , eqVer "1.3.00.25560-1"
        , eqVer "150_28-3"
        , eqVer "1.0.r15.g3fc772c-5"
        , eqVer "0.88-2"
        ]
      ]
    , testGroup "Lenses and Traversals"
      [ testCase "SemVer - Increment Patch" incPatch
      , testCase "SemVer - Increment Patch from Text" incFromT
      , testCase "SemVer - Get patches" patches
      ]
    , testGroup "Megaparsec Behaviour"
      [ testCase "manyTill" $ parse nameGrab "manyTill" "linux-firmware-3.2.14-1-x86_64.pkg.tar.xz" @?= Right "linux-firmware"
      , testCase "Extracting version" $ parse versionGrab "extraction" "linux-firmware-3.2.14-1-x86_64.pkg.tar.xz" @?= Right(Ideal $ SemVer 3 2 14 [[Digits 1]] [])
      ]
    ]
  ]

compVer :: T.Text -> T.Text -> TestTree
compVer a b = testCase (printf "%s < %s" a b) $ comp versioning a b

eqVer :: T.Text -> TestTree
eqVer a = testCase (T.unpack a) $ equal versioning a

-- | Does pretty-printing return a Versioning to its original form?
isomorph :: T.Text -> Assertion
isomorph t = case prettyV <$> versioning t of
  Right t' -> t @?= t'
  Left e   -> assertBool (errorBundlePretty e) False

-- | Does pretty-printing return a Version to its original form?
isomorphV :: T.Text -> Assertion
isomorphV t = case prettyVer <$> version t of
  Right t' -> t @?= t'
  Left e   -> assertBool (errorBundlePretty e) False

-- | Does pretty-printing return a SemVer to its original form?
isomorphSV :: T.Text -> Assertion
isomorphSV t = case prettySemVer <$> semver t of
  Right t' -> t @?= t'
  Left e   -> assertBool (errorBundlePretty e) False

isomorphPVP :: T.Text -> Assertion
isomorphPVP t = case prettyPVP <$> pvp t of
  Right t' -> t @?= t'
  Left e   -> assertBool (errorBundlePretty e) False

isomorphM :: T.Text -> Assertion
isomorphM t = case prettyMess <$> mess t of
  Right t' -> t @?= t'
  Left e   -> assertBool (errorBundlePretty e) False

comp :: Ord b => (T.Text -> Either a b) -> T.Text -> T.Text -> Assertion
comp f a b = check $ (<) <$> f a <*> f b

equal :: Ord r => (T.Text -> Either l r) -> T.Text -> Assertion
equal f a = check $ (\r -> r == r) <$> f a

check :: Either a Bool -> Assertion
check = assertBool "Some Either-based assertion failed" . either (const False) id

isSemVer :: Versioning -> Bool
isSemVer (Ideal _) = True
isSemVer _         = False

isVersion :: Versioning -> Bool
isVersion (General _) = True
isVersion _           = False

isMess :: Versioning -> Bool
isMess (Complex _) = True
isMess _           = False

incPatch :: Assertion
incPatch = (v1 & patch %~ (+ 1)) @?= v2
  where v1 = Ideal $ SemVer 1 2 3 [] []
        v2 = Ideal $ SemVer 1 2 4 [] []

incFromT :: Assertion
incFromT = (("1.2.3" :: T.Text) & patch %~ (+ 1)) @?= "1.2.4"

patches :: Assertion
patches = ps @?= [3,4,5]
  where ps = (["1.2.3","2.3.4","3.4.5"] :: [T.Text]) ^.. each . patch

main :: IO ()
main = defaultMain suite

nameGrab :: Parsec Void T.Text T.Text
nameGrab = T.pack <$> manyTill anySingle (try finished)
  where finished = char '-' *> lookAhead digitChar

versionGrab :: Parsec Void T.Text Versioning
versionGrab = manyTill anySingle (try finished) *> ver
  where finished = char '-' *> lookAhead digitChar
        ver = fmap Ideal semver' <|> fmap General version' <|> fmap Complex mess'

hush :: Either a b -> Maybe b
hush (Left _)  = Nothing
hush (Right b) = Just b
