# Normalize intensities across samples using smooth Quantile Normalization (qsmooth)

This function performs a smooth Quantile Normalization on each sub-group
in the data set (qsmooth). **It therefore requires grouping
information**. See Examples for more information. This approach might
perform better than the standard approach,
[`normalize_quantile_all`](https://yasche.github.io/metamorphr/reference/normalize_quantile_all.md),
if sub-groups are very different (e.g., when comparing cancer vs. normal
tissue). The result lies somewhere between
[`normalize_quantile_group`](https://yasche.github.io/metamorphr/reference/normalize_quantile_group.md)
and
[`normalize_quantile_all`](https://yasche.github.io/metamorphr/reference/normalize_quantile_all.md).
Basically a re-implementation of Hicks *et al.* (2018).

## Usage

``` r
normalize_quantile_smooth(
  data,
  group_column = .data$Group,
  rolling_window = 0.05
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- group_column:

  Which column should be used for grouping? Usually
  `grouping_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- rolling_window:

  `normalize_quantile_smooth` uses a rolling window median to eliminate
  isolated outliers. This argument specifies the size of the rolling
  window as a fraction of the number of unique features in `data`. For
  example, if there are 100 features in `data` and
  `rolling_window = 0.05`, the rolling median will be calculated from 5
  features. Set `rolling_window = 0` to disable.

## Value

A tibble with intensities normalized across samples.

## References

- S. C. Hicks, K. Okrah, J. N. Paulson, J. Quackenbush, R. A.
  Irizarry, H. C. Bravo, *Biostatistics* **2018**, *19*, 185–198, DOI
  10.1093/biostatistics/kxx028.

- Y. Zhao, L. Wong, W. W. B. Goh, *Sci Rep* **2020**, *10*, 15534, DOI
  10.1038/s41598-020-72664-6.

## Examples

``` r
toy_metaboscape %>%
  # Metadata, including grouping information, must be added before using normalize_quantile_group()
  join_metadata(toy_metaboscape_metadata) %>%
  normalize_quantile_smooth(group_column = Group)
#> # A tibble: 110 × 12
#>      UID Feature      Sample Intensity    RT `m/z` Name  Formula Group Replicate
#>    <int> <chr>        <chr>      <dbl> <dbl> <dbl> <chr> <chr>   <chr>     <int>
#>  1     1 161.10519 D… Sampl…    NaN     0.45  162. NA    C7H15N… cont…         1
#>  2     2 276.13647 D… Sampl…      2.56  0.45  277. Octy… C16H22… cont…         1
#>  3     3 304.24023 D… Sampl…    NaN     0.55  305. Arac… C20H32… cont…         1
#>  4     4 417.23236 D… Sampl…    NaN     1     418. NA    NA      cont…         1
#>  5     5 104.10753 D… Sampl…    NaN     2.84  105. NA    C5H14NO cont…         1
#>  6     6 105.04259 D… Sampl…    NaN     3.33  106. NA    C3H8NO3 cont…         1
#>  7     7 237.09204 D… Sampl…    NaN     5.22  238. Keta… C13H16… cont…         1
#>  8     8 745.09111 D… Sampl…      2.56  6.37  746. NADPH C21H30… cont…         1
#>  9     9 427.02942 D… Sampl…    NaN     7.08  428. ADP   C10H15… cont…         1
#> 10    10 1284.34904 … Sampl…    NaN     8.32 1285. NA    NA      cont…         1
#> # ℹ 100 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
