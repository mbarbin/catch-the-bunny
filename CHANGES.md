## 0.0.6 (unreleased)

### Added

### Changed

- Upgrade `ocaml` to `5.2`.
- Upgrade `dune` to `3.16`.

### Deprecated

### Fixed

### Removed

## 0.0.5 (2024-03-13)

### Changed

- Uses `expect-test-helpers` (reduce core dependencies)
- Run `ppx_js_style` as a linter & make it a `dev` dependency.
- Upgrade GitHub workflows `actions/checkout` to v4.
- In CI, specify build target `@all`, and add `@lint`.
- List ppxs instead of `ppx_jane`.

## 0.0.4 (2024-02-14)

### Added

- New tests to increase code coverage.

### Changed

- Upgrade dune to `3.14`.
- Build the doc with sherlodoc available to enable the doc search bar.

## 0.0.3 (2024-02-09)

### Changed

- Internal changes related to the release process.
- Upgrade dune and internal dependencies.
- Improve `bisect_ppx` setup for test coverage.

## 0.0.2 (2024-01-18)

### Changed

- Generate opam file from `dune-project`.
- Other internal changes related to the release process.

## 0.0.1 (2023-11-01)

- Initial version, solve problem for sizes 6 and 8.
