# metamorphr (development version)

## Fixes
* Fixed critical bug in `read_featuretable` where empty metadata columns caused issues if `metadata_col`s were supplied as numerics.

# metamorphr 0.4.0

## New features
* New `read_featuretable_mzmine()` to read "full_feature_table" CSV files created with 'mzmine' into a tidy tibble
* New `convert_from_wide()` to convert wide feature tables to tidy tibbles
* New `convert_from_matrix()` to convert a matrix to a tidy tibble
* New `remove_empty_cols()` to remove empty columns (i.e., columns that _only_ contain `NA`) from a tibble or data frame
* `read_featuretable()` and `read_featuretable_mzmine()` now have a `remove_empty_cols` argument

# metamorphr 0.3.0

## New features
* `read_mgf()` now supports tab-separated values
* New `msn_scale()` to scale MSn spectra to the peaks with the highest intensity

## Lifecycle changes
### Newly deprecated
* `calc_neutral_loss()` is deprecated and replaced by `msn_calc_nl()` for naming consistency

# metamorphr 0.2.0

## New features
* Added `formula_to_mass()` (experimental)
* Added functions related to the Kendrick mass: `calc_kmd()`, `calc_km()` and `calc_nominal_km()`

## Fixes
* Fixed typo in DESCRIPTION
* Fixed two tests that caused an error on some operating systems on CRAN

# metamorphr 0.1.1

* Fixed CRAN comments.

# metamorphr 0.1.0

* Initial CRAN submission.
