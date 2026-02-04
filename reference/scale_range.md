# Scale intensities of features using range scaling

Scales the intensities of all features using

\$\$\widetilde{x}\_{ij}=\frac{x\_{ij}-\overline{x}\_{i}}{x\_{i,max}-x\_{i,min}}\$\$

where \\\widetilde{x}\_{ij}\\ is the intensity of sample \\j\\, feature
\\i\\ after scaling, \\x\_{ij}\\ is the intensity of sample \\j\\,
feature \\i\\ before scaling, \\\overline{x}\_{i}\\ is the mean of
intensities of feature \\i\\ across all samples, \\x\_{i,max}\\ is the
maximum intensity of feature \\i\\ across all samples and \\x\_{i,min}\\
is the minimum intensity of feature \\i\\ across all samples. In other
words, it subtracts the mean intensity of a feature across samples from
the intensities of that feature in each sample and divides by the range
of that feature. For more information, see the reference section.

## Usage

``` r
scale_range(data)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

## Value

A tibble with range scaled intensities.

## References

- R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K.
  Smilde, M. J. Van Der Werf, *BMC Genomics* **2006**, *7*, 142, DOI
  10.1186/1471-2164-7-142.

## Examples

``` r
toy_metaboscape %>%
  scale_range()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1   -0.25    0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1   -0.260   0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1   NA       0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1   -0.113   1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1   -0.0714  2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1    0.0476  3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1   NA       5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1   -0.0625  6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1   -0.178   7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1   NA       8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
