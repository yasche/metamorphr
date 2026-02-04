# Join a featuretable and sample metadata

Joins a featuretable and associated sample metadata. Basically a wrapper
around
[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
where `by = "Sample"`.

## Usage

``` r
join_metadata(data, metadata)
```

## Arguments

- data:

  A feature table created with
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md)

- metadata:

  Sample metadata created with
  [`create_metadata_skeleton`](https://yasche.github.io/metamorphr/reference/create_metadata_skeleton.md)

## Value

A tibble with added sample metadata.

## Examples

``` r
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata)
#> # A tibble: 110 × 12
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
#> 10    10 1284.34904 … Sampl…        NA  8.32 1285. NA    NA      cont…         1
#> # ℹ 100 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
