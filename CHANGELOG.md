# Changelog

## 6.0.8 (2025-02-03)

#### Changed

- Bumped upper bound on `base` (support for GHC 9.12).

## 6.0.7 (2024-06-03)

#### Changed

- Bumped upper bound on `base`.

## 6.0.6 (2024-03-08)

#### Fixed

- Account for large numbers when parsing on 32-bit (or smaller) systems.

## 6.0.5 (2024-01-24)

#### Fixed

- Certain illegal versions were parsing as PVP.

## 6.0.4 (2023-12-29)

#### Changed

- Bump dependencies to support GHC 9.8.

## 6.0.3 (2023-10-23)

#### Added

- `Data` instances for the various data types.
- Simple conversion functions between the main version types.
- Compile-time constructors via Template Haskell, like `versioningQ`.

## 6.0.2 (2023-10-12)

#### Added

- `Lift` instances for the various types, which allows parsing version numbers
  at compile time within Template Haskell quotes. Currently there is no exported
  function that supports this directly, but you could write one like:

```haskell
-- | Parse a `Versioning` at compile time.
thVer :: Text -> Q Exp
thVer nm =
  case versioning nm of
    Left err -> fail (errorBundlePretty err)
    Right v  -> lift v
```

#### Changed

- Due to the new dependency on `template-haskell`, GHC 8.8 is now the lowest
  supported compiler version.

## 6.0.1 (2023-05-08)

#### Fixed

- Restored the ability to compile with GHC versions earlier than 9.

## 6.0.0 (2023-04-29)

A number of type changes have been made to improve parsing and comparison logic.
Doing so fixed several bugs and made the code cleaner overall.

If you're just doing basic parsing and comparisons and not actually inspecting
the types themselves, you shouldn't notice a difference.

#### Added

- New types `Release`, `Chunks`, and `Chunk`.

#### Changed

- Both `SemVer` and `Version` now contain a better-behaving `Release` type for their prerelease info.
- Similarly, `Version` now also has a better-behaving `Chunks` type for its main
  version number sections.
- The `release` traversal now yields a `Maybe Release`.
- Versions with `~` in their metadata will now parse as a `Mess`. Example: `12.0.0-3ubuntu1~20.04.5`

#### Removed

- The various `Semigroup` instances. Adding version numbers together is a
  nonsensical operation and should never have been added in the first place.
- The `VChunk` and `VUnit` types and their associated functions.

#### Fixed

- Leading zeroes are handled a little better in `SemVer` pre-release data.

## 5.0.5 (2023-03-23)

#### Changed

- Bumped `base` bound to support GHC 9.6.

## 5.0.4 (2022-10-18)

#### Changed

- Bumped `base` bound to support GHC 9.4.

## 5.0.3 (2022-02-25)

#### Fixed

- A bug in `prettyVer` that flipped the order of the `preRel` and `meta` fields.

## 5.0.2 (2022-01-21)

#### Added

- `text-2.0` support.

## 5.0.1 (2021-12-08)

#### Changed

- Support for GHC 9.2.

#### Fixed

- Remove redundant pattern match.

## 5.0.0 (2021-04-14)

This release brings `versions` in line with version `2.0.0` of the SemVer spec.
The main addition to the spec is the allowance of hyphens in both the prerelease
and metadata sections. As such, **certain versions like `1.2.3+1-1` which
previously would not parse as SemVer now do.**

To accomodate this and other small spec updates, the `SemVer` and `Version`
types have received breaking changes here.

#### Changed

- **Breaking:** The `_svMeta` field of `SemVer` is now parsed as a dumber `Maybe Text` instead of `[VChunk]`, due to metadata now being allowed to possess
  leading zeroes.
- **Breaking:** Like the above, the `_vMeta` field of `Version` is now `Maybe Text`.
- **Breaking: The `_vRel` and `_vMeta` fields of `Version` have had their order
  flipped.** Further, the prelease and meta sections are now expected in the
  same order as `SemVer` when parsing (prerel first, meta second). `Version` is
  thus now a quite similar to `SemVer`, except allowing letters in more
  permissive positions.
- **Breaking:** The `meta` traversal has been altered to accomodate the metadata
  field changes.

#### Fixed

- Parsing certain legal SemVers specified in the spec.

## 4.0.3 (2021-02-23)

#### Changed

- Support for GHC 9.

## 4.0.2 (2021-01-23)

#### Fixed

- A bug in zero parsing within SemVer prereleases. [#42]

[#42]: https://github.com/fosskers/versions/issues/42

## 4.0.1 (2020-10-22)

#### Fixed

- An infinite loop in `Version` comparison. [aura#652]

[aura#652]: https://github.com/fosskers/aura/issues/652

## 4.0.0 (2020-10-20)

#### Changed

- **Breaking:** `VChunk` now cannot be empty.
- **Breaking:** A `Version` now guarantees `NonEmpty` chunks.
- **Breaking:** A `Mess` now guarantees `NonEmpty` chunks, and its structure has
  been significantly changed. Particularly, `Mess` values are now aware of the
  `Int` values they hold (when they do), as well as "revision" values of the
  pattern `rXYZ`.
- Comparison of `Version` values is more memory efficient.

#### Added

- `Version` now has an extra field, `_vMeta :: [VChunk]` for capturing
  "metadata" like Semver. This prevents otherwise nice-looking versions from
  being demoted to `Mess`.
- The `MChunk` type to accomodate the changes to `Mess` mentioned above.

#### Removed

- **Breaking:** `Version` no longer has a `Monoid` instance.

#### Fixed

- `""` no longer parses in any way. [#32]
- Version strings with trailing whitespace no longer parse via `versioning`. [#33]
- Particular edge cases involving `Mess` comparisons. [aura#646]
- A particular edge case involving prereleases in `Version` comparisons. [aura#586]

[#32]: https://github.com/fosskers/versions/issues/32
[#33]: https://github.com/fosskers/versions/issues/33
[aura#646]: https://github.com/fosskers/aura/issues/646
[aura#586]: https://github.com/fosskers/aura/issues/586

## 3.5.4 (2020-05-12)

#### Added

- The functions `isIdeal`, `isGeneral`, and `isComplex` for `Bool`-based
  inspection of parse results.
- `messMajor`, `messMinor`, `messPatch`, and `messPatchChunk` for improved
  introspection into `Mess` values.

#### Changed

- Improved `Mess` comparison logic.

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

- Added support for _epoch_ numbers in the `Version` type. These are numbers
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
