# Filter Features based on the absolute number or fraction of samples it was found in

Filters features based on the number or fraction of samples they are
found in. This is usually one of the first steps in metabolomics data
analysis and often already performed when the feature table is first
created from the raw spectral files..

## Usage

``` r
filter_global_mv(data, min_found = 0.5, fraction = TRUE)
```

## Arguments

- data:

  A tidy tibble created by
  [`metamorphr::read_featuretable()`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- min_found:

  In how many samples must a Feature be found? If `fraction == TRUE`, a
  value between 0 and 1 (*e.g.*, 0.5 if a Feature must be found in at
  least half the samples). If `fraction == FALSE` the absolute maximum
  number of samples (*e.g.*, 5 if a specific Feature must be found in at
  least 5 samples).

- fraction:

  Either `TRUE` or `FALSE`. Should `min_found` be the absolute number of
  samples or a fraction?

## Value

A filtered tibble.

## Examples

``` r
# Example 1: A feature must be found in at least 50 % of the samples
toy_metaboscape %>%
  filter_global_mv(min_found = 0.5)
#> # A tibble: 88 × 8
#>      UID Feature               Sample  Intensity    RT `m/z` Name        Formula
#>    <int> <chr>                 <chr>       <dbl> <dbl> <dbl> <chr>       <chr>  
#>  1     1 161.10519 Da 26.98 s  Sample1         4  0.45  162. NA          C7H15N…
#>  2     2 276.13647 Da 27.28 s  Sample1         3  0.45  277. Octyl hydr… C16H22…
#>  3     3 304.24023 Da 32.86 s  Sample1        NA  0.55  305. Arachidoni… C20H32…
#>  4     4 417.23236 Da 60.08 s  Sample1         5  1     418. NA          NA     
#>  5     5 104.10753 Da 170.31 s Sample1         5  2.84  105. NA          C5H14NO
#>  6     6 105.04259 Da 199.80 s Sample1         5  3.33  106. NA          C3H8NO3
#>  7     8 745.09111 Da 382.23 s Sample1         3  6.37  746. NADPH       C21H30…
#>  8     9 427.02942 Da 424.84 s Sample1         4  7.08  428. ADP         C10H15…
#>  9     1 161.10519 Da 26.98 s  Sample2         3  0.45  162. NA          C7H15N…
#> 10     2 276.13647 Da 27.28 s  Sample2         6  0.45  277. Octyl hydr… C16H22…
#> # ℹ 78 more rows

# Example 2: A feature must be found in at least 8 samples
toy_metaboscape %>%
  filter_global_mv(min_found = 8, fraction = FALSE)
#> # A tibble: 88 × 8
#>      UID Feature               Sample  Intensity    RT `m/z` Name        Formula
#>    <int> <chr>                 <chr>       <dbl> <dbl> <dbl> <chr>       <chr>  
#>  1     1 161.10519 Da 26.98 s  Sample1         4  0.45  162. NA          C7H15N…
#>  2     2 276.13647 Da 27.28 s  Sample1         3  0.45  277. Octyl hydr… C16H22…
#>  3     3 304.24023 Da 32.86 s  Sample1        NA  0.55  305. Arachidoni… C20H32…
#>  4     4 417.23236 Da 60.08 s  Sample1         5  1     418. NA          NA     
#>  5     5 104.10753 Da 170.31 s Sample1         5  2.84  105. NA          C5H14NO
#>  6     6 105.04259 Da 199.80 s Sample1         5  3.33  106. NA          C3H8NO3
#>  7     8 745.09111 Da 382.23 s Sample1         3  6.37  746. NADPH       C21H30…
#>  8     9 427.02942 Da 424.84 s Sample1         4  7.08  428. ADP         C10H15…
#>  9     1 161.10519 Da 26.98 s  Sample2         3  0.45  162. NA          C7H15N…
#> 10     2 276.13647 Da 27.28 s  Sample2         6  0.45  277. Octyl hydr… C16H22…
#> # ℹ 78 more rows
```
