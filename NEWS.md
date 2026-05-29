# metamorphr (development version)

## New features
* New `read_featuretable_mzmine()` to read "full_feature_table" CSV files created with 'mzmine' into a tidy tibble
* New `convert_from_wide()` to convert wide feature tables to tidy tibbles
* New `convert_from_matrix()` to convert a matrix to a tidy tibble

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
