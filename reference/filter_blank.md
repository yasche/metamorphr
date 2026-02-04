# Filter Features based on their occurrence in blank samples

Filters Features based on their occurrence in blank samples. For
example, if `min_frac = 3` the maximum intensity in samples must be at
least 3 times as high as in blanks for a Feature not to be filtered out.

## Usage

``` r
filter_blank(
  data,
  blank_samples,
  min_frac = 3,
  blank_as_group = FALSE,
  group_column = NULL
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- blank_samples:

  Defines the blanks. If `blank_as_group = FALSE` a character vector
  containing the names of the blank samples as in the `Sample` column of
  `data`. If `blank_as_group = TRUE` the name(s) of the group(s) that
  define blanks, as in the `Group` column of `data`. The latter can only
  be used if sample metadata is provided.

- min_frac:

  A numeric defining how many times higher the maximum intensity in
  samples must be in relation to blanks.

- blank_as_group:

  A logical indicating if `blank_samples` are the names of samples or
  group(s).

- group_column:

  Only relevant if `blank_as_group = TRUE`. Which column should be used
  for grouping blank and non-blank samples? Usually
  `group_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

## Value

A filtered tibble.

## Examples

``` r
# Example 1: Define blanks by sample name
toy_metaboscape %>%
  filter_blank(blank_samples = c("Blank1", "Blank2"), blank_as_group = FALSE, min_frac = 3)
#> # A tibble: 77 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1         4  0.45  162. NA         C7H15N…
#>  2     3 304.24023 Da 32.86 s   Sample1        NA  0.55  305. Arachidon… C20H32…
#>  3     5 104.10753 Da 170.31 s  Sample1         5  2.84  105. NA         C5H14NO
#>  4     7 237.09204 Da 313.24 s  Sample1        NA  5.22  238. Ketamine   C13H16…
#>  5     8 745.09111 Da 382.23 s  Sample1         3  6.37  746. NADPH      C21H30…
#>  6     9 427.02942 Da 424.84 s  Sample1         4  7.08  428. ADP        C10H15…
#>  7    10 1284.34904 Da 498.94 s Sample1        NA  8.32 1285. NA         NA     
#>  8     1 161.10519 Da 26.98 s   Sample2         3  0.45  162. NA         C7H15N…
#>  9     3 304.24023 Da 32.86 s   Sample2         2  0.55  305. Arachidon… C20H32…
#> 10     5 104.10753 Da 170.31 s  Sample2         6  2.84  105. NA         C5H14NO
#> # ℹ 67 more rows

# Example 2: Define blanks by group name
# toy_metaboscape %>%
#  join_metadata(toy_metaboscape_metadata) %>%
#  filter_blank(blank_samples = "blank",
#               blank_as_group = TRUE,
#               min_frac = 3,
#               group_column = Group)
```
