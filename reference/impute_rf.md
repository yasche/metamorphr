# Impute missing values using random forest

Basically a wrapper function around
`missForest::`[`missForest`](https://rdrr.io/pkg/missForest/man/missForest.html).
Imputes missing values using the random forest algorithm.

## Usage

``` r
impute_rf(data, random_seed = 1L, ...)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- random_seed:

  A seed for the random number generator. Can be an integer or `NULL`
  (in case no particular seed should be used) but for reproducibility
  reasons it is **strongly advised** to provide an integer.

- ...:

  Additional parameters passed to
  [`missForest`](https://rdrr.io/pkg/missForest/man/missForest.html).

## Value

A tibble with imputed missing values.

## References

- [missForest](https://CRAN.R-project.org/package=missForest) on CRAN

- D. J. Stekhoven, P. Bühlmann, *Bioinformatics* **2012**, *28*,
  112–118, DOI 10.1093/bioinformatics/btr597.

## Examples

``` r
toy_metaboscape %>%
  impute_rf()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1      4     0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1      3     0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1      5.19  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1      5     1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1      5     2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1      5     3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1      8.33  5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1      3     6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1      4     7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1      3.3   8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
