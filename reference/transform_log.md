# Transforms the intensities by calculating their log

Log-transforms intensities. The default (base = 10) calculates the
log10. This transformation can help reduce heteroscedasticity. See
references for more information.

## Usage

``` r
transform_log(data, base = 10)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- base:

  Which base should be used for the log-transformation. The default (10)
  means that log10 values of the intensities are calculated.

## Value

A tibble with log-transformed intensities.

## References

- R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K.
  Smilde, M. J. Van Der Werf, *BMC Genomics* **2006**, *7*, 142, DOI
  10.1186/1471-2164-7-142.

## Examples

``` r
toy_metaboscape %>%
  impute_lod() %>%
  transform_log()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1     0.602  0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1     0.477  0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1    -0.699  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1     0.699  1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1     0.699  2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1     0.699  3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1     0.146  5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1     0.477  6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1     0.602  7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1    -0.398  8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
