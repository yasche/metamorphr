# Impute missing values by replacing them with the Feature mean

Replace missing intensity values (`NA`) with the Feature mean of
non-`NA` values. For example, if a Feature has the measured intensities
`NA, 1, NA, 3, 2` in samples 1-5, the intensities after `impute_mean()`
would be `2, 1, 2, 3, 2`.

## Usage

``` r
impute_mean(data)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

## Value

A tibble with imputed missing values.

## Examples

``` r
toy_metaboscape %>%
  impute_mean()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1      4     0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1      3     0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1      5.25  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1      5     1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1      5     2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1      5     3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1      8.33  5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1      3     6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1      4     7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1      3.33  8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
