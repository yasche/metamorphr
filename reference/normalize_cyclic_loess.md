# Normalize intensities across samples using cyclic LOESS normalization

The steps the algorithm takes are the following:

1.  log2 transform the intensities

2.  Choose 2 samples to generate an
    [MA-plot](https://en.wikipedia.org/wiki/MA_plot) from

3.  Fit a LOESS curve

4.  Subtract half of the difference between the predicted value and the
    true value from the intensity of sample 1 and add the same amount to
    the intensity of Sample 2

5.  Repeat for all unique combinations of samples

6.  Repeat all steps until the model converges or `n_iter` is reached.

Convergence is assumed if the confidence intervals of all LOESS smooths
include the 0 line. If `fixed_iter = TRUE`, the algorithm will perform
exactly `n_iter` iterations. If `fixed_iter = FALSE`, the algorithm will
perform a maximum of `n_iter` iterations.

See the reference section for details.

## Usage

``` r
normalize_cyclic_loess(
  data,
  n_iter = 3,
  fixed_iter = TRUE,
  loess_span = 0.7,
  level = 0.95,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- n_iter:

  The number of iterations to perform. If `fixed_iter = TRUE` exactly
  `n_iter` will be performed. If `fixed_iter = FALSE` a maximum of
  `n_iter` will be performed and the algorithm will stop whether
  convergence is reached or not.

- fixed_iter:

  Should a fixed number of iterations be performed?

- loess_span:

  The span of the LOESS fit. A larger span produces a smoother line.

- level:

  The confidence level for the convergence criterion. Note that a a
  larger confidence level produces larger confidence intervals and
  therefore the algorithm stops earlier.

- verbose:

  `TRUE` or `FALSE`. Should messages be printed to the console?

- ...:

  Arguments passed onto [`loess`](https://rdrr.io/r/stats/loess.html).
  For example,
  `degree = 1, family = "symmetric", iterations = 4, surface = "direct"`
  produces a LOWESS fit.

## Value

A tibble with intensities normalized across samples.

## References

- B. M. Bolstad, R. A. Irizarry, M. Åstrand, T. P. Speed,
  *Bioinformatics* **2003**, *19*, 185–193, DOI
  10.1093/bioinformatics/19.2.185.

- Karla Ballman, Diane Grill, Ann Oberg, Terry Therneau, “Faster cyclic
  loess: normalizing DNA arrays via linear models” can be found under
  https://www.mayo.edu/research/documents/biostat-68pdf/doc-10027897,
  2004.

- K. V. Ballman, D. E. Grill, A. L. Oberg, T. M. Therneau,
  *Bioinformatics* **2004**, *20*, 2778–2786, DOI
  10.1093/bioinformatics/bth327.

## Examples

``` r
toy_metaboscape %>%
  impute_lod() %>%
  normalize_cyclic_loess()
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1     2.85   0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1     4.31   0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1     1.77   0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1     3.76   1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1     2.36   2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1     2.46   3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1     2.09   5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1     1.72   6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1     3.08   7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1     0.705  8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
