{-# LANGUAGE OverloadedStrings #-}

module Test where

import Data.Either
import Data.Monoid ((<>))
import Data.Text (Text,unpack)
import Distribution.Version
import Test.Tasty
import Test.Tasty.HUnit

---

-- | These don't need to parse as a SemVer.
goodVers :: [Text]
goodVers = [ "1", "1.2", "1.58.0-3",  "44.0.2403.157-1"
           , "0.25-2",  "8.u51-1", "21-2", "7.1p1-1", "20150826-1"
           , "1.2.3-alpha.2"
           ]

messes :: [Text]
messes = [ "10.2+0.93+1-1", "003.03-3", "002.000-7", "1:0.10.16-3"
         , "20.26.1_0-2"
         ]

badSemVs :: [Text]
badSemVs = [ "1", "1.2", "1.2.3+a1b2bc3.1-alpha.2", "a.b.c", "1.01.1"
           , "1.2.3+a1b!2c3.1"
           ]

goodSemVs :: [Text]
goodSemVs = [ "1.2.3", "1.2.3-1", "1.2.3-alpha", "1.2.3-alpha.2"
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

suite :: TestTree
suite = testGroup "Unit Tests"
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
      map (\s -> testCase (unpack s) $ isomorph s) goodVers
    , testGroup "Comparisons" $
      map (\(a,b) -> testCase (unpack $ a <> " < " <> b) $ comp version a b) $
      zip cabalOrd (tail cabalOrd) <> zip semverOrd (tail semverOrd)
    ]
  , testGroup "(Complex) Mess" []
  ]

-- | Does pretty-printing return a Version to its original form?
isomorph :: Text -> Assertion
isomorph t = Right t @=? (prettyVer <$> version t)

-- | Does pretty-printing return a SemVer to its original form?
isomorphSV :: Text -> Assertion
isomorphSV t = Right t @=? (prettySemVer <$> semver t)

comp :: Ord b => (Text -> Either a b) -> Text -> Text -> Assertion
comp f a b = assert $ either (const False) id $ (<) <$> f a <*> f b

{-}
-- Need to submit patch for these, as well as Maybe instance.
assertRight :: String -> Either a b -> Assertion
assertRight _ (Right _)  = return ()
assertRight msg (Left _) = assertFailure msg

instance Assertable (Either a b) where
  assert = assertRight ""

assertMaybe :: String -> Maybe a -> Assertion
assertMaybe _ (Just _)  = return ()
assertMaybe msg Nothing = assertFailure msg

instance Assertable (Maybe a) where
  assert = assertMaybe ""
-}

main :: IO ()
main = defaultMain suite
