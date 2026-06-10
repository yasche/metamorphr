# Changelog

## metamorphr (development version)

## metamorphr 0.4.1

### Fixes

- Fixed critical bug in `read_featuretable` where empty metadata columns
  caused issues if `metadata_col`s were supplied as numerics.

## metamorphr 0.4.0

CRAN release: 2026-06-09

### New features

- New
  [`read_featuretable_mzmine()`](https://yasche.github.io/metamorphr/reference/read_featuretable_mzmine.md)
  to read “full_feature_table” CSV files created with ‘mzmine’ into a
  tidy tibble
- New
  [`convert_from_wide()`](https://yasche.github.io/metamorphr/reference/convert_from_wide.md)
  to convert wide feature tables to tidy tibbles
- New
  [`convert_from_matrix()`](https://yasche.github.io/metamorphr/reference/convert_from_matrix.md)
  to convert a matrix to a tidy tibble
- New
  [`remove_empty_cols()`](https://yasche.github.io/metamorphr/reference/remove_empty_cols.md)
  to remove empty columns (i.e., columns that *only* contain `NA`) from
  a tibble or data frame
- [`read_featuretable()`](https://yasche.github.io/metamorphr/reference/read_featuretable.md)
  and
  [`read_featuretable_mzmine()`](https://yasche.github.io/metamorphr/reference/read_featuretable_mzmine.md)
  now have a `remove_empty_cols` argument

## metamorphr 0.3.0

CRAN release: 2026-03-04

### New features

- [`read_mgf()`](https://yasche.github.io/metamorphr/reference/read_mgf.md)
  now supports tab-separated values
- New
  [`msn_scale()`](https://yasche.github.io/metamorphr/reference/msn_scale.md)
  to scale MSn spectra to the peaks with the highest intensity

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
