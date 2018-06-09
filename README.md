versions
========

[![Hackage](https://img.shields.io/hackage/v/versions.svg?style=flat)](https://hackage.haskell.org/package/versions)
[![Stackage Nightly](http://stackage.org/package/versions/badge/nightly)](http://stackage.org/nightly/package/versions)
[![Stackage LTS](http://stackage.org/package/versions/badge/lts)](http://stackage.org/lts/package/versions)

A Haskell library for parsing and comparing software version numbers.

About
-----
We like to give version numbers to our software in a myriad of ways. Some
ways follow strict guidelines for incrementing and comparison. Some follow
conventional wisdom and are generally self-consistent. Some are just plain
asinine. This library provides a means of parsing and comparing *any* style
of versioning, be it a nice Semantic Version like this:

> 1.2.3-r1+git123

...or a monstrosity like this:

> 2:10.2+0.0093r3+1-1

Please switch to [Semantic Versioning](http://semver.org) if you aren't
currently using it. It provides consistency in version incrementing and has
the best constraints on comparisons.

Usage
-----
In general, `versioning` is the function you want. It attempts to parse a given
Text using the three individual parsers, `semver`, `version` and `mess`. If
one fails, it tries the next. If you know you only want to parse one
specific version type, use that parser directly (e.g. `semver`).

#### Lenses and Traversals
The parse result types have Lenses/Traversals for accessing their data
fields. For instance, to increment the patch number of a parsed SemVer, you
could:

```haskell
incPatch :: SemVer -> SemVer
incPatch s = s & patch %~ (+ 1)
```

Or, something more involved:

```haskell
-- | Get all major versions of legally parsed SemVers.
majors :: [Text] -> [Word]
majors vs = vs ^.. each . to semver . _Right . major
```

The `to semver . _Right` is clunky, so we provide some direct `Text`
Traverals inspired by
([micro](http://hackage.haskell.org/package/microlens-aeson))
[lens-aeson](http://hackage.haskell.org/package/lens-aeson):

```haskell
-- | Get the major version of any `Text` that has one.
majors :: [Text] -> [Word]
majors vs = vs ^.. each . major
```

We can also use these `Text` Traversals to increment versions, as above:

```haskell
incPatch :: Text -> Text
incPatch s = s & patch %~ (+ 1)

> incPatch "1.2.3"
"1.2.4"
```
