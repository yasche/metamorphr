# Normalize intensities across samples by dividing by the sample median

Normalize across samples by dividing feature intensities by the sample
median, making the median 1 in all samples. See References for more
information.

## Usage

``` r
normalize_median(data)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

## Value

A tibble with intensities normalized across samples.

## References

T. Ramirez, A. Strigun, A. Verlohner, H.-A. Huener, E. Peter, M. Herold,
N. Bordag, W. Mellert, T. Walk, M. Spitzer, X. Jiang, S. Sperber, T.
Hofmann, T. Hartung, H. Kamp, B. Van Ravenzwaay, *Arch Toxicol*
**2018**, *92*, 893–906, DOI 10.1007/s00204-017-2079-6.

## Examples

``` r
toy_metaboscape %>%
  normalize_median()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1      1     0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1      0.75  0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1     NA     0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1      1.25  1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1      1.25  2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1      1.25  3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1     NA     5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1      0.75  6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1      1     7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1     NA     8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
