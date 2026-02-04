# Scale intensities of features using grouped vast scaling

A variation of
[`scale_vast`](https://yasche.github.io/metamorphr/reference/scale_vast.md)
but uses a group-specific coefficient of variation and therefore
requires group information. See
[`scale_vast`](https://yasche.github.io/metamorphr/reference/scale_vast.md)
and the References section for more information.

## Usage

``` r
scale_vast_grouped(data, group_column = .data$Group)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- group_column:

  Which column should be used for grouping? Usually
  `grouping_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

## Value

A tibble with vast scaled intensities.

## References

- R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K.
  Smilde, M. J. Van Der Werf, *BMC Genomics* **2006**, *7*, 142, DOI
  10.1186/1471-2164-7-142.

## Examples

``` r
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  scale_vast_grouped()
#> # A tibble: 110 × 12
#>      UID Feature      Sample Intensity    RT `m/z` Name  Formula Group Replicate
#>    <int> <chr>        <chr>      <dbl> <dbl> <dbl> <chr> <chr>   <chr>     <int>
#>  1     1 161.10519 D… Sampl…    -4.09   0.45  162. NA    C7H15N… cont…         1
#>  2     2 276.13647 D… Sampl…    -2.24   0.45  277. Octy… C16H22… cont…         1
#>  3     3 304.24023 D… Sampl…    NA      0.55  305. Arac… C20H32… cont…         1
#>  4     4 417.23236 D… Sampl…    -1.23   1     418. NA    NA      cont…         1
#>  5     5 104.10753 D… Sampl…    -1.13   2.84  105. NA    C5H14NO cont…         1
#>  6     6 105.04259 D… Sampl…     1.06   3.33  106. NA    C3H8NO3 cont…         1
#>  7     7 237.09204 D… Sampl…    NA      5.22  238. Keta… C13H16… cont…         1
#>  8     8 745.09111 D… Sampl…    -0.585  6.37  746. NADPH C21H30… cont…         1
#>  9     9 427.02942 D… Sampl…    -2.63   7.08  428. ADP   C10H15… cont…         1
#> 10    10 1284.34904 … Sampl…    NA      8.32 1285. NA    NA      cont…         1
#> # ℹ 100 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
