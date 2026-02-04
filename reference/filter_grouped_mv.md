# Group-based feature filtering

Similar to
[`filter_global_mv`](https://yasche.github.io/metamorphr/reference/filter_global_mv.md)
it filters features that are found in a specified number of samples. The
key difference is that `filter_grouped_mv()` takes groups into
consideration and therefore needs sample metadata. For example, if
`fraction = TRUE` and `min_found = 0.5`, a feature must be found in at
least 50 % of the samples of at least 1 group. It is very similar to the
*Filter features by occurrences in groups* option in Bruker MetaboScape.

## Usage

``` r
filter_grouped_mv(
  data,
  min_found = 0.5,
  group_column = .data$Group,
  fraction = TRUE
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md)
  with added sample metadata. See
  ?[`create_metadata_skeleton`](https://yasche.github.io/metamorphr/reference/create_metadata_skeleton.md)
  for help.

- min_found:

  Defines in how many samples of at least 1 group a Feature must be
  found not to be filtered out. If `fraction == TRUE`, a value between 0
  and 1 (*e.g.*, 0.5 if a Feature must be found in at least half the
  samples of at least 1 group). If `fraction == FALSE` the absolute
  maximum number of samples (*e.g.*, 5 if a specific Feature must be
  found in at least 5 samples of at least 1 group).

- group_column:

  Which column should be used for grouping? Usually
  `group_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- fraction:

  Either `TRUE` or `FALSE`. Should `min_found` be the absolute number of
  samples or a fraction?

## Value

A filtered tibble.

## Examples

``` r
# A Feature must be found in all samples of at least 1 group.
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  filter_grouped_mv(min_found = 1, group_column = Group)
#> # A tibble: 99 × 12
#>      UID Feature      Sample Intensity    RT `m/z` Name  Formula Group Replicate
#>    <int> <chr>        <chr>      <dbl> <dbl> <dbl> <chr> <chr>   <chr>     <int>
#>  1     1 161.10519 D… Sampl…         4  0.45  162. NA    C7H15N… cont…         1
#>  2     2 276.13647 D… Sampl…         3  0.45  277. Octy… C16H22… cont…         1
#>  3     3 304.24023 D… Sampl…        NA  0.55  305. Arac… C20H32… cont…         1
#>  4     4 417.23236 D… Sampl…         5  1     418. NA    NA      cont…         1
#>  5     5 104.10753 D… Sampl…         5  2.84  105. NA    C5H14NO cont…         1
#>  6     6 105.04259 D… Sampl…         5  3.33  106. NA    C3H8NO3 cont…         1
#>  7     7 237.09204 D… Sampl…        NA  5.22  238. Keta… C13H16… cont…         1
#>  8     8 745.09111 D… Sampl…         3  6.37  746. NADPH C21H30… cont…         1
#>  9     9 427.02942 D… Sampl…         4  7.08  428. ADP   C10H15… cont…         1
#> 10     1 161.10519 D… Sampl…         3  0.45  162. NA    C7H15N… cont…         2
#> # ℹ 89 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
