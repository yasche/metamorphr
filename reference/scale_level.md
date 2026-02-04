# Scale intensities of features using level scaling

Scales the intensities of all features using

\$\$\widetilde{x}\_{ij}=\frac{x\_{ij}-\overline{x}\_{i}}{\overline{x}\_{i}}\$\$

where \\\widetilde{x}\_{ij}\\ is the intensity of sample \\j\\, feature
\\i\\ after scaling, \\x\_{ij}\\ is the intensity of sample \\j\\,
feature \\i\\ before scaling and \\\overline{x}\_{i}\\ is the mean of
intensities of feature \\i\\ across all samples

In other words, it performs centering
([`scale_center`](https://yasche.github.io/metamorphr/reference/scale_center.md))
and divides by the feature mean, thereby focusing on the relative
intensity.

## Usage

``` r
scale_level(data)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

## Value

A tibble with level scaled intensities.

## References

- R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K.
  Smilde, M. J. Van Der Werf, *BMC Genomics* **2006**, *7*, 142, DOI
  10.1186/1471-2164-7-142.

## Examples

``` r
toy_metaboscape %>%
  impute_lod() %>%
  scale_level()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1   -0.0393  0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1   -0.377   0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1   -0.948   0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1   -0.0709  1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1    0.217   2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1    0.297   3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1   -0.575   5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1    0.260   6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1   -0.0265  7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1   -0.667   8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
