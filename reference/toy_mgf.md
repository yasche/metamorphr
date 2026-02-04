# A small toy data set containing MSn spectra

Data was generated with
[`metamorphr::read_mgf()`](https://yasche.github.io/metamorphr/reference/read_mgf.md)
and can be reproduced with This tibble can be reproduced with
`metamorphr::read_mgf(system.file("extdata", "toy_mgf.mgf", package = "metamorphr"))`.

## Usage

``` r
toy_mgf
```

## Format

### `toy_mgf`

A data frame with 3 rows and 5 columns:

- VARIABLEONE:

  A fictional variable.

- VARIABLETWO:

  A fictional variable.

- VARIABLETHREE:

  A fictional variable.

- PEPMASS:

  The precursor ion m/z.

- MSn:

  A list column containing MSn spectra.

## Source

This data set contains fictional data!
