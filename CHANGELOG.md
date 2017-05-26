Changelog
=========

3.0.2
-----

- Expose internal parsers so that they could be used in other parser programs
  that parse version numbers in larger files.

3.0.0
-----
- Updated for `megaparsec-5` and `ghc-8`

2.0.0
-----
- Switched to `megaparsec` to perform all parsing as `Text`
- Support for legacy `String` removed
- Added more Traversals and INLINE'd all Lenses/Traversals

1.1.0
-----
- Added Lenses and Traversals (no `lens` dependency)
