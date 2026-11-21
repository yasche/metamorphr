# Convert a tidy tibble into a format compatible with the "Functional Analysis" module in 'MetaboAnalyst'

Creates a tibble with columns `m.z`, `p.value`, `t.score` and optionally
`r.t` to use in the "Functional Analysis" module in 'MetaboAnalyst'.
Metrics are calculated using a
[`t.test`](https://rdrr.io/r/stats/t.test.html).

## Usage

``` r
convert_to_fa(
  data,
  m_z_col,
  rt_col = NULL,
  group_column,
  groups_to_compare,
  batch_column = NULL,
  batch = NULL,
  adjust_p = FALSE,
  log2_before = FALSE,
  ...
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- m_z_col:

  Which column holds the precursor m/z? Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- rt_col:

  Optional. Which column holds the precursor retention times? Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- group_column:

  Which column should be used for grouping? Usually
  `group_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- groups_to_compare:

  Names of the groups which should be compared as a character vector.
  Those are the group names in the `group_column`. They are usually
  provided in the form of a metadata tibble and joined via
  [`join_metadata`](https://yasche.github.io/metamorphr/reference/join_metadata.md).

- batch_column:

  Which column contains the batch information? Usually
  `batch_column = Batch`. Only relevant if `data` contains multiple
  batches. For example, if `data` contains 2 batches and each batch
  contains measurements of separate controls, `group_column` and `batch`
  arguments should be provided. Otherwise controls of both batches will
  be considered when calculating the *p*-value and log2 fold change.
  Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- batch:

  The names of the batch(es) that should be included when calculating
  *p*-value and log2 fold change.

- adjust_p:

  Should the *p*-value be adjusted? Can be either `FALSE`, (the default)
  in case no adjustment should be made or any or the name from
  [`p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html) (e.g.,
  `adjust_p = "fdr"`).

- log2_before:

  A logical. Should the data be log2 transformed prior to calculating
  the *p*-values?

- ...:

  Arguments passed on to
  [`t.test`](https://rdrr.io/r/stats/t.test.html). If none are provided
  (the default), a Welch Two Sample *t*-test will be performed.

## Value

A tibble with columns `m.z`, `p.value`, `t.score` and optionally `r.t`.

## Examples

``` r
# Without optional retention time column
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  convert_to_fa(group_column = Group,
                groups_to_compare = c("control", "treatment"),
                return_tbl = TRUE,
                alternative = "two.sided",
                m_z_col = `m/z`)
#> # A tibble: 10 × 3
#>      m.z  p.value t.score
#>    <dbl>    <dbl>   <dbl>
#>  1  162.  0.0175   -8.04 
#>  2  277.  0.200     1.58 
#>  3  305.  0.0111  -10.3  
#>  4  418.  0.798     0.311
#>  5  105.  0.182    -1.75 
#>  6  106.  0.781     0.311
#>  7  238. NA        NA    
#>  8  746.  0.561    -0.655
#>  9  428.  0.00106  -8.49 
#> 10 1285. NA        NA    

# With optional retention time column
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  convert_to_fa(group_column = Group,
                groups_to_compare = c("control", "treatment"),
                return_tbl = TRUE,
                m_z_col = `m/z`,
                rt_col = RT)
#> # A tibble: 10 × 4
#>      m.z  p.value t.score   r.t
#>    <dbl>    <dbl>   <dbl> <dbl>
#>  1  162.  0.0175   -8.04   0.45
#>  2  277.  0.200     1.58   0.45
#>  3  305.  0.0111  -10.3    0.55
#>  4  418.  0.798     0.311  1   
#>  5  105.  0.182    -1.75   2.84
#>  6  106.  0.781     0.311  3.33
#>  7  238. NA        NA      5.22
#>  8  746.  0.561    -0.655  6.37
#>  9  428.  0.00106  -8.49   7.08
#> 10 1285. NA        NA      8.32
```
