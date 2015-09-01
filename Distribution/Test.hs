{-# LANGUAGE OverloadedStrings #-}

module Distribution.Test where

import Data.Either
import Data.String
import Data.Text (Text)
import Distribution.Version
import Test.Tasty
import Test.Tasty.HUnit

---

simplest :: IsString s => s
simplest = "1"

simple :: IsString s => s
simple = "1.2"

semver0 :: IsString s => s
semver0 = "1.2.3"

semver1 :: IsString s => s
semver1 = "1.2.3-1"

semver2 :: IsString s => s
semver2 = "1.2.3-alpha"

semver3 :: IsString s => s
semver3 = "1.2.3-alpha.2"

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "General Parser"
    [ testCase "1" $ roundabout simplest
    , testCase "1.2.3" $ roundabout semver0
    ]
  , testGroup "Semantic Versioning"
    [ testGroup "Bad Versions (shouldn't parse)"
      [ testCase simplest $ assert $ isLeft $ semver simplest
      , testCase simple $ assert $ isLeft $ semver simple
      ]
    , testGroup "Good Versions (should parse)"
      [ testCase semver0 $ roundaboutSV semver0
      , testCase semver1 $ roundaboutSV semver1
      , testCase semver2 $ roundaboutSV semver2
      , testCase semver3 $ roundaboutSV semver3
      ]
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
