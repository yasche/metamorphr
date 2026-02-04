# Impute missing values using nearest neighbor averaging

Basically a wrapper function around
`impute::`[`impute.knn`](https://rdrr.io/pkg/impute/man/impute.knn.html).
Imputes missing values using the k-th nearest neighbor algorithm.

Note that the function ln-transforms the data prior to imputation and
transforms it back to the original scale afterwards. **Please do not do
it manually prior to calling `impute_knn()`!** See References for more
information.

**Important Note**

`impute_knn()` depends on the `impute` package from Bioconductor. If
`metamorphr` was installed via
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html),
dependencies from Bioconductor were not automatically installed. When
`impute_knn()` is called without the `impute` package installed, you
should be asked if you want to install `pak` and `impute`. If you want
to use `impute_knn()` you have to install those. In case you run into
trouble with the automatic installation, please install `impute`
manually. See [impute: Imputation for microarray
data](https://bioconductor.org/packages/release/bioc/html/impute.html)
for instructions on manual installation.

## Usage

``` r
impute_knn(data, quietly = TRUE, ...)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- quietly:

  `TRUE` or `FALSE`. Should messages and warnings from
  [`impute.knn`](https://rdrr.io/pkg/impute/man/impute.knn.html) be
  printed to the console?

- ...:

  Additional parameters passed to
  [`impute.knn`](https://rdrr.io/pkg/impute/man/impute.knn.html).

## Value

A tibble with imputed missing values.

## References

- Robert Tibshirani, Trevor Hastie, **2017**, DOI
  10.18129/B9.BIOC.IMPUTE.

- J. Khan, J. S. Wei, M. Ringnér, L. H. Saal, M. Ladanyi, F.
  Westermann, F. Berthold, M. Schwab, C. R. Antonescu, C.
  Peterson, P. S. Meltzer, *Nat Med* **2001**, *7*, 673–679, DOI
  10.1038/89044.

## Examples

``` r
toy_metaboscape %>%
  impute_knn()
#> Warning: 2 rows with more than 50 % entries missing;
#>  mean imputation used for these rows
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1      4     0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1      3     0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1      4.04  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1      5     1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1      5     2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1      5     3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1      4.05  5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1      3     6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1      4     7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1      4.05  8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
