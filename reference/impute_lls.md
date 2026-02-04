# Impute missing values using Local Least Squares (LLS)

Basically a wrapper around
`pcaMethods::`[`llsImpute`](https://rdrr.io/pkg/pcaMethods/man/llsImpute.html).
For a detailed discussion, see the `vignette("pcaMethods")` and
[`vignette("missingValues", "pcaMethods")`](https://bioconductor.org/packages/release/bioc/vignettes/pcaMethods/inst/doc/missingValues.pdf)
as well as the References section.

*Important Note* `impute_lls()` depends on the `pcaMethods` package from
Bioconductor. If `metamorphr` was installed via
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html),
dependencies from Bioconductor were not automatically installed. When
[`impute_svd()`](https://yasche.github.io/metamorphr/reference/impute_svd.md)
is called without the `pcaMethods` package installed, you should be
asked if you want to install `pak` and `pcaMethods`. If you want to use
`impute_lls()` you have to install those. In case you run into trouble
with the automatic installation, please install `pcaMethods` manually.
See [pcaMethods – a Bioconductor package providing PCA methods for
incomplete
data](https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html)
for instructions on manual installation.

## Usage

``` r
impute_lls(
  data,
  correlation = "pearson",
  complete_genes = FALSE,
  center = FALSE,
  cluster_size = 10
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- correlation:

  The method used to calculate correlations between features. One of
  `"pearson"`, `"spearman"` or `"kendall"`. See
  [`cor`](https://rdrr.io/r/stats/cor.html).

- complete_genes:

  If `TRUE` only complete features will be used for regression, if
  `FALSE`, all will be used.

- center:

  Should `data` be mean centered? See
  [`prep`](https://rdrr.io/pkg/pcaMethods/man/prep.html) for details.

- cluster_size:

  The number of similar features used for regression.

## Value

A tibble with imputed missing values.

## References

- H. R. Wolfram Stacklies, **2017**, DOI 10.18129/B9.BIOC.PCAMETHODS.

- W. Stacklies, H. Redestig, M. Scholz, D. Walther, J. Selbig,
  *Bioinformatics* **2007**, *23*, 1164–1167, DOI
  10.1093/bioinformatics/btm069.

## Examples

``` r
# The cluster size must be reduced because
# the data set is too small for the default (10)

toy_metaboscape %>%
  impute_lls(complete_genes = TRUE, cluster_size = 5)
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1      4     0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1      3     0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1      2.86  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1      5     1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1      5     2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1      5     3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1      3.94  5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1      3     6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1      4     7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1      2.87  8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
