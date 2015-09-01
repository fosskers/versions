{-# LANGUAGE OverloadedStrings #-}

module Distribution.Test where

import Data.Either
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
badSemVs = [ "1", "1.2", "1.2.3+a1b2bc3.1-alpha.2" ]

goodSemVs :: [Text]
goodSemVs = [ "1.2.3", "1.2.3-1", "1.2.3-alpha", "1.2.3-alpha.2"
          , "1.2.3+a1b2c3.1", "1.2.3-alpha.2+a1b2c3.1" ]

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "General Parser"
    [ testCase simplest $ roundabout simplest
    , testCase simple $ roundabout simple
    , testCase "1.2.3" $ roundabout "1.2.3"
    ]
  , testGroup "Semantic Versioning"
    [ testGroup "Bad Versions (shouldn't parse)" $
      map (\s -> testCase (unpack s) $ assert $ isLeft $ semver s) badSemVs
    , testGroup "Good Versions (should parse)" $
      map (\s -> testCase (unpack s) $ roundaboutSV s) goodSemVs
    ]
  ]

-- | Does pretty-printing return a Version to its original form?
roundabout :: Text -> Assertion
roundabout t = Right t @=? (prettyVer <$> version t)

-- | Does pretty-printing return a SemVer to its original form?
roundaboutSV :: Text -> Assertion
roundaboutSV t = Right t @=? (prettySemVer <$> semver t)

run :: IO ()
run = defaultMain suite
