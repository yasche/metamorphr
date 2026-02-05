# Changelog

## metamorphr (development version)

### New features

- [`read_mgf()`](https://yasche.github.io/metamorphr/reference/read_mgf.md)
  now supports tab-separated values
- Added
  [`msn_scale()`](https://yasche.github.io/metamorphr/reference/msn_scale.md)

### Lifecycle changes

#### Newly deprecated

- [`calc_neutral_loss()`](https://yasche.github.io/metamorphr/reference/calc_neutral_loss.md)
  is deprecated and replaced by
  [`msn_calc_nl()`](https://yasche.github.io/metamorphr/reference/msn_calc_nl.md)
  for naming consistency

## metamorphr 0.2.0

CRAN release: 2025-10-09

### New features

- Added
  [`formula_to_mass()`](https://yasche.github.io/metamorphr/reference/formula_to_mass.md)
  (experimental)
- Added functions related to the Kendrick mass:
  [`calc_kmd()`](https://yasche.github.io/metamorphr/reference/calc_kmd.md),
  [`calc_km()`](https://yasche.github.io/metamorphr/reference/calc_km.md)
  and
  [`calc_nominal_km()`](https://yasche.github.io/metamorphr/reference/calc_nominal_km.md)

### Fixes

- Fixed typo in DESCRIPTION
- Fixed two tests that caused an error on some operating systems on CRAN

## metamorphr 0.1.1

CRAN release: 2025-09-01

- Fixed CRAN comments.

## metamorphr 0.1.0

- Initial CRAN submission.
