# Changelog

## Unreleased

#### Added

- The functions `isIdeal`, `isGeneral`, and `isComplex` for `Bool`-based
  inspection of parse results.
- `messMajor`, `messMinor`, `messPatch`, and `messPatchChunk` for improved
  introspection into `Mess` values.

## 3.5.3
- GHC 8.10 support.

## 3.5.2
- Added a new `PVP` type and parsers.

## 3.5.1.1
- GHC 8.8 compatibility.

## 3.5.0
- Updated to `megaparsec-7`. Our `ParsingError` type alias has changed to match
  Megaparsec's new error model, and `errorBundlePretty` is now exposed instead of
  the old `parseErrorPretty`.

## 3.4.0.1
- Enhanced the whitespace handling in `semver'`, `version'`, and `mess'`.

## 3.4.0
- Removed `ParseV` and surrounding machinery.
  Use `versioning` now instead of the `parseV` function.

## 3.3.2
- GHC 8.4.1 compatibility.

## 3.3.0
- New `Semantic` typeclass that provides Traversals for SemVer-like data out
  of all the version types. `Text` was also given an instance, so its much
  easier to manipulate directly:

```
λ "1.2.3" & minor %~ (+ 1)
"1.3.3"
```

Some Lenses and Traversals had their names changed or were removed entirely
to accomodate this new typeclass.

- `SemVer` and `Version` should never contain negative values, so their numeric
  components were changed from `Int` to `Word`.

## 3.2.0
- Updated for `megaparsec-6` and GHC 8.2.

## 3.1.1
- Added instances for common typeclasses: `Generic`, `NFData`, and
  `Hashable`. This is to avoid having users define these instances themselves
  as orphans. If there are more instances you want added, please let me know.
  `Data` was left out on purpose.

## 3.1.0
- Added support for *epoch* numbers in the `Version` type. These are numbers
  like the `1:` in `1:2.3.4`. These are used in Arch Linux in rare cases where
  packages change their versioning scheme, but need a reliable integer prefix
  to establish ordering. The `Version` type has been given a new field,
  `_vEpoch :: Maybe Int`, and a corresponding lens, `vEpoch`.

## 3.0.2
- Expose internal parsers so that they could be used in other parser programs
  that parse version numbers in larger files.

## 3.0.0
- Updated for `megaparsec-5` and `ghc-8`

## 2.0.0
- Switched to `megaparsec` to perform all parsing as `Text`
- Support for legacy `String` removed
- Added more Traversals and INLINE'd all Lenses/Traversals

## 1.1.0
- Added Lenses and Traversals (no `lens` dependency)
