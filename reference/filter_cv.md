# Filter Features based on their coefficient of variation

Filters Features based on their coefficient of variation (CV). The CV is
defined as \\CV = \frac{s_i}{\overline{x_i}}\\ with \\s_i\\ = Standard
deviation of sample \\i\\ and \\\overline{x_i}\\ = Mean of sample \\i\\.

## Usage

``` r
filter_cv(
  data,
  reference_samples,
  max_cv = 0.2,
  ref_as_group = FALSE,
  group_column = NULL,
  na_as_zero = TRUE
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- reference_samples:

  The names of the samples or group which will be used to calculate the
  CV of a feature. Usually Quality Control samples.

- max_cv:

  The maximum allowed CV. 0.2 is a reasonable start.

- ref_as_group:

  A logical indicating if `reference_samples` are the names of samples
  or group(s).

- group_column:

  Only relevant if `ref_as_group = TRUE`. Which column should be used
  for grouping reference and non-reference samples? Usually
  `group_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- na_as_zero:

  Should `NA` be replaced with 0 prior to calculation? Under the hood
  `filter_cv` calculates the CV by
  `stats::sd(..., na.rm = TRUE) / mean(..., na.rm = TRUE)`. If there are
  3 samples to calculate the CV from and 2 of them are `NA` for a
  specific feature, then the CV for that Feature will be `NA` if
  `na_as_zero = FALSE`. This might lead to problems. `na_as_zero = TRUE`
  is the safer pick. Zeros will be replaced with `NA` after calculation
  no matter if it is `TRUE` or `FALSE`.

## Value

A filtered tibble.

## References

[Coefficient of Variation on
Wikipedia](https://en.wikipedia.org/wiki/Coefficient_of_variation)

## Examples

``` r
# Example 1: Define reference samples by sample names
toy_metaboscape %>%
  filter_cv(max_cv = 0.2, reference_samples = c("QC1", "QC2", "QC3"))
#> # A tibble: 33 × 8
#>      UID Feature               Sample  Intensity    RT `m/z` Name        Formula
#>    <int> <chr>                 <chr>       <dbl> <dbl> <dbl> <chr>       <chr>  
#>  1     3 304.24023 Da 32.86 s  Sample1        NA  0.55  305. Arachidoni… C20H32…
#>  2     8 745.09111 Da 382.23 s Sample1         3  6.37  746. NADPH       C21H30…
#>  3     9 427.02942 Da 424.84 s Sample1         4  7.08  428. ADP         C10H15…
#>  4     3 304.24023 Da 32.86 s  Sample2         2  0.55  305. Arachidoni… C20H32…
#>  5     8 745.09111 Da 382.23 s Sample2         4  6.37  746. NADPH       C21H30…
#>  6     9 427.02942 Da 424.84 s Sample2         3  7.08  428. ADP         C10H15…
#>  7     3 304.24023 Da 32.86 s  Sample3         1  0.55  305. Arachidoni… C20H32…
#>  8     8 745.09111 Da 382.23 s Sample3         2  6.37  746. NADPH       C21H30…
#>  9     9 427.02942 Da 424.84 s Sample3         4  7.08  428. ADP         C10H15…
#> 10     3 304.24023 Da 32.86 s  Sample4         8  0.55  305. Arachidoni… C20H32…
#> # ℹ 23 more rows

# Example 2: Define reference samples by group name
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  filter_cv(max_cv = 0.2, reference_samples = "QC", ref_as_group = TRUE, group_column = Group)
#> # A tibble: 33 × 12
#>      UID Feature      Sample Intensity    RT `m/z` Name  Formula Group Replicate
#>    <int> <chr>        <chr>      <dbl> <dbl> <dbl> <chr> <chr>   <chr>     <int>
#>  1     3 304.24023 D… Sampl…        NA  0.55  305. Arac… C20H32… cont…         1
#>  2     8 745.09111 D… Sampl…         3  6.37  746. NADPH C21H30… cont…         1
#>  3     9 427.02942 D… Sampl…         4  7.08  428. ADP   C10H15… cont…         1
#>  4     3 304.24023 D… Sampl…         2  0.55  305. Arac… C20H32… cont…         2
#>  5     8 745.09111 D… Sampl…         4  6.37  746. NADPH C21H30… cont…         2
#>  6     9 427.02942 D… Sampl…         3  7.08  428. ADP   C10H15… cont…         2
#>  7     3 304.24023 D… Sampl…         1  0.55  305. Arac… C20H32… cont…         3
#>  8     8 745.09111 D… Sampl…         2  6.37  746. NADPH C21H30… cont…         3
#>  9     9 427.02942 D… Sampl…         4  7.08  428. ADP   C10H15… cont…         3
#> 10     3 304.24023 D… Sampl…         8  0.55  305. Arac… C20H32… trea…         1
#> # ℹ 23 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
