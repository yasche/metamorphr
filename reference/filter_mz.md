# Filter Features based on their mass-to-charge ratios

Facilitates filtering by given mass-to-charge ratios (m/z) with a
defined tolerance. Can also be used to filter based on exact mass.

## Usage

``` r
filter_mz(data, m_z_col, masses, tolerance = 5, tolerance_type = "ppm")
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- m_z_col:

  Which column holds the precursor m/z (or exact mass)? Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- masses:

  The mass(es) to filter by.

- tolerance:

  A numeric. The tolerance to apply to the masses Either an absolute
  value in Da (if `tolerance_type = "absolute"`) or in ppm (if
  `tolerance_type = "ppm"`).

- tolerance_type:

  Either `"absolute"` or `"ppm"`. Should the tolerance be an absolute
  value or in ppm?

## Value

A filtered tibble.

## Examples

``` r
# Use a tolerance of plus or minus 5 ppm
toy_metaboscape %>%
  filter_mz(m_z_col = `m/z`, 162.1132, tolerance = 5, tolerance_type = "ppm")
#> # A tibble: 11 × 8
#>      UID Feature              Sample  Intensity    RT `m/z` Name  Formula 
#>    <int> <chr>                <chr>       <dbl> <dbl> <dbl> <chr> <chr>   
#>  1     1 161.10519 Da 26.98 s Sample1         4  0.45  162. NA    C7H15NO3
#>  2     1 161.10519 Da 26.98 s Sample2         3  0.45  162. NA    C7H15NO3
#>  3     1 161.10519 Da 26.98 s Sample3         4  0.45  162. NA    C7H15NO3
#>  4     1 161.10519 Da 26.98 s Sample4        NA  0.45  162. NA    C7H15NO3
#>  5     1 161.10519 Da 26.98 s Sample5         9  0.45  162. NA    C7H15NO3
#>  6     1 161.10519 Da 26.98 s Sample6         8  0.45  162. NA    C7H15NO3
#>  7     1 161.10519 Da 26.98 s QC1             7  0.45  162. NA    C7H15NO3
#>  8     1 161.10519 Da 26.98 s QC2             6  0.45  162. NA    C7H15NO3
#>  9     1 161.10519 Da 26.98 s QC3             3  0.45  162. NA    C7H15NO3
#> 10     1 161.10519 Da 26.98 s Blank1         NA  0.45  162. NA    C7H15NO3
#> 11     1 161.10519 Da 26.98 s Blank2         NA  0.45  162. NA    C7H15NO3

# Use a tolerance of plus or minus 0.005 Da
toy_metaboscape %>%
  filter_mz(m_z_col = `m/z`, 162.1132, tolerance = 0.005, tolerance_type = "absolute")
#> # A tibble: 11 × 8
#>      UID Feature              Sample  Intensity    RT `m/z` Name  Formula 
#>    <int> <chr>                <chr>       <dbl> <dbl> <dbl> <chr> <chr>   
#>  1     1 161.10519 Da 26.98 s Sample1         4  0.45  162. NA    C7H15NO3
#>  2     1 161.10519 Da 26.98 s Sample2         3  0.45  162. NA    C7H15NO3
#>  3     1 161.10519 Da 26.98 s Sample3         4  0.45  162. NA    C7H15NO3
#>  4     1 161.10519 Da 26.98 s Sample4        NA  0.45  162. NA    C7H15NO3
#>  5     1 161.10519 Da 26.98 s Sample5         9  0.45  162. NA    C7H15NO3
#>  6     1 161.10519 Da 26.98 s Sample6         8  0.45  162. NA    C7H15NO3
#>  7     1 161.10519 Da 26.98 s QC1             7  0.45  162. NA    C7H15NO3
#>  8     1 161.10519 Da 26.98 s QC2             6  0.45  162. NA    C7H15NO3
#>  9     1 161.10519 Da 26.98 s QC3             3  0.45  162. NA    C7H15NO3
#> 10     1 161.10519 Da 26.98 s Blank1         NA  0.45  162. NA    C7H15NO3
#> 11     1 161.10519 Da 26.98 s Blank2         NA  0.45  162. NA    C7H15NO3
```
