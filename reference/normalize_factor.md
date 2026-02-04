# Normalize intensities across samples using a normalization factor

Normalization is done by dividing the intensity by a sample-specific
factor (e.g., weight, protein or DNA content). This function requires a
sample-specific factor, usually supplied via the `Factor` column from
the sample metadata. See the Examples section for details.

## Usage

``` r
normalize_factor(data, factor_column = .data$Factor)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- factor_column:

  Which column contains the sample-specific factor? Usually
  `factor_column = Factor`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

## Value

A tibble with intensities normalized across samples.

## Examples

``` r
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  normalize_factor()
#> # A tibble: 110 × 12
#>      UID Feature      Sample Intensity    RT `m/z` Name  Formula Group Replicate
#>    <int> <chr>        <chr>      <dbl> <dbl> <dbl> <chr> <chr>   <chr>     <int>
#>  1     1 161.10519 D… Sampl…      4.66  0.45  162. NA    C7H15N… cont…         1
#>  2     2 276.13647 D… Sampl…      3.50  0.45  277. Octy… C16H22… cont…         1
#>  3     3 304.24023 D… Sampl…     NA     0.55  305. Arac… C20H32… cont…         1
#>  4     4 417.23236 D… Sampl…      5.83  1     418. NA    NA      cont…         1
#>  5     5 104.10753 D… Sampl…      5.83  2.84  105. NA    C5H14NO cont…         1
#>  6     6 105.04259 D… Sampl…      5.83  3.33  106. NA    C3H8NO3 cont…         1
#>  7     7 237.09204 D… Sampl…     NA     5.22  238. Keta… C13H16… cont…         1
#>  8     8 745.09111 D… Sampl…      3.50  6.37  746. NADPH C21H30… cont…         1
#>  9     9 427.02942 D… Sampl…      4.66  7.08  428. ADP   C10H15… cont…         1
#> 10    10 1284.34904 … Sampl…     NA     8.32 1285. NA    NA      cont…         1
#> # ℹ 100 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
