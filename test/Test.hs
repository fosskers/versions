{-# LANGUAGE OverloadedStrings #-}

module Test where

import Data.Either
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text,unpack)
import Distribution.Version
import Test.Tasty
import Test.Tasty.HUnit

---

simplest :: IsString s => s
simplest = "1"

simple :: IsString s => s
simple = "1.2"

badSemVs :: [Text]
badSemVs = [ "1", "1.2", "1.2.3+a1b2bc3.1-alpha.2", "a.b.c", "1.01.1"
           , "1.2.3+a1b!2c3.1" ]

goodSemVs :: [Text]
goodSemVs = [ "1.2.3", "1.2.3-1", "1.2.3-alpha", "1.2.3-alpha.2"
            , "1.2.3+a1b2c3.1", "1.2.3-alpha.2+a1b2c3.1" ]

-- | The exact example from `http://semver.org`
toCompare :: [Text]
toCompare = [ "1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta"
            , "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1"
            , "1.0.0" ]

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "General Parser"
    [ testCase simplest $ isometric simplest
    , testCase simple $ isometric simple
    , testCase "1.2.3" $ isometric "1.2.3"
    ]
  , testGroup "Semantic Versioning"
    [ testGroup "Bad Versions (shouldn't parse)" $
      map (\s -> testCase (unpack s) $ assert $ isLeft $ semver s) badSemVs
    , testGroup "Good Versions (should parse)" $
      map (\s -> testCase (unpack s) $ isometricSV s) goodSemVs
    , testGroup "Comparisons" $
      map (\(a,b) -> testCase (unpack $ a <> " < " <> b) $ comp a b) $
      zip toCompare (tail toCompare)
    ]
  ]

-- | Does pretty-printing return a Version to its original form?
isometric :: Text -> Assertion
isometric t = Right t @=? (prettyVer <$> version t)

-- | Does pretty-printing return a SemVer to its original form?
isometricSV :: Text -> Assertion
isometricSV t = Right t @=? (prettySemVer <$> semver t)

comp :: Text -> Text -> Assertion
comp a b = assert $ either (const False) id $ (<) <$> semver a <*> semver b

{-}
-- Need to submit patch for these, as well as Maybe instance.
assertRight :: String -> Either a b -> Assertion
assertRight _ (Right _)  = return ()
assertRight msg (Left _) = assertFailure msg

instance Assertable (Either a b) where
  assert = assertRight ""
-}

main :: IO ()
main = defaultMain suite
