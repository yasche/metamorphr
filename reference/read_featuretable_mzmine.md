# Read a 'full_feature_table' from 'mzmine' into a tidy tibble

Similar to
[`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md)
but specifically for full_feature_table' files created with 'mzmine'.
For more information, see [the 'mzmine'
documentation](https://mzmine.github.io/mzmine_documentation/module_docs/io/feat-list-export.html).

## Usage

``` r
read_featuretable_mzmine(
  file,
  intensity = "height",
  field_separator = ",",
  label_col = 1,
  import_datafile_cols = FALSE,
  remove_empty_cols = FALSE,
  show_removed_cols = TRUE
)
```

## Arguments

- file:

  A path to a file but can also be a connection or literal data.

- intensity:

  A character that specifies what should be used as the
  (semi-)quantitative measure. Either `"height"` or `"area"`.

- field_separator:

  The field separator as specified in 'mzmine'. Usually `","` if the
  file is in common CSV format.

- label_col:

  The index or name of the column that will be used to label Features.
  For example an identifier (*e.g.*, KEGG, CAS, HMDB) or a *m/z*-RT
  pair.

- import_datafile_cols:

  Should columns that begin with `datafile:` be imported? Those columns
  contain sample-specific information, for example the retention time of
  a feature measured in a specific sample. Usually, this information is
  not necessary for downstream analysis but it can be used for quaility
  control purposes. If `TRUE`, `datafile:` columns are imported and the
  sample names are removed from the column names. This allows for tidy
  storage of the information in one column per variable.

- remove_empty_cols:

  Either `TRUE` or `FALSE`. Should empty columns be removed after
  reading the feature table? For a more fine-grained control, you can
  use a combination of
  [`read_delim`](https://readr.tidyverse.org/reference/read_delim.html),
  [`remove_empty_cols`](https://yasche.github.io/metamorphr/reference/remove_empty_cols.md)
  and
  [`convert_from_wide`](https://yasche.github.io/metamorphr/reference/convert_from_wide.md).See
  the respective function documentation for more details.

- show_removed_cols:

  Only relevant if `remove_empty_cols = TRUE`. If `TRUE` prints a
  message that shows which columns were removed.

## Value

A tidy tibble.

## References

- H. Wickham, *J. Stat. Soft.* **2014**, *59*, DOI
  10.18637/jss.v059.i10.

- H. Wickham, M. Averick, J. Bryan, W. Chang, L. McGowan, R.
  François, G. Grolemund, A. Hayes, L. Henry, J. Hester, M. Kuhn, T.
  Pedersen, E. Miller, S. Bache, K. Müller, J. Ooms, D. Robinson, D.
  Seidel, V. Spinu, K. Takahashi, D. Vaughan, C. Wilke, K. Woo, H.
  Yutani, *JOSS* **2019**, *4*, 1686, DOI 10.21105/joss.01686.

- “12 Tidy data \| R for Data Science,” can be found under
  <https://r4ds.had.co.nz/tidy-data.html>, **2023**.

## Examples

``` r
# Read a toy dataset in the format produced with mzmine.

featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")

# Example 1: Use feature height as the metric
featuretable <- read_featuretable_mzmine(
  featuretable_path,
  intensity = "height"
)

featuretable
#> # A tibble: 20 × 54
#>      UID Feature Sample  Intensity    mz `mz_range:min` `mz_range:max`    rt
#>    <int> <chr>   <chr>       <dbl> <dbl>          <dbl>          <dbl> <dbl>
#>  1     1 1       Sample1      809   218.           218.           218. 0.207
#>  2     2 2       Sample1      951.  161.           161.           161. 0.179
#>  3     3 5       Sample1      352   153.           153.           153. 0.194
#>  4     4 8       Sample1     2174   285.           285.           285. 0.206
#>  5     5 9       Sample1     2871   257.           257.           257. 0.256
#>  6     1 1       Sample2      670   218.           218.           218. 0.207
#>  7     2 2       Sample2     1071   161.           161.           161. 0.179
#>  8     3 5       Sample2      261   153.           153.           153. 0.194
#>  9     4 8       Sample2     1404   285.           285.           285. 0.206
#> 10     5 9       Sample2     3278   257.           257.           257. 0.256
#> 11     1 1       Sample3      927   218.           218.           218. 0.207
#> 12     2 2       Sample3      766   161.           161.           161. 0.179
#> 13     3 5       Sample3      501   153.           153.           153. 0.194
#> 14     4 8       Sample3     1466   285.           285.           285. 0.206
#> 15     5 9       Sample3     1792   257.           257.           257. 0.256
#> 16     1 1       Sample4      751   218.           218.           218. 0.207
#> 17     2 2       Sample4     1073   161.           161.           161. 0.179
#> 18     3 5       Sample4      376   153.           153.           153. 0.194
#> 19     4 8       Sample4     2161   285.           285.           285. 0.206
#> 20     5 9       Sample4     3074   257.           257.           257. 0.256
#> # ℹ 46 more variables: `rt_range:min` <dbl>, `rt_range:max` <dbl>,
#> #   `ion_mobility_range:min` <dbl>, `ion_mobility_range:max` <dbl>, ccs <dbl>,
#> #   ion_mobility_unit <chr>, area <dbl>, height <dbl>,
#> #   `intensity_range:min` <dbl>, `intensity_range:max` <dbl>, charge <dbl>,
#> #   fragment_scans <dbl>, `alignment_scores:rate` <dbl>,
#> #   `alignment_scores:aligned_features_n` <dbl>,
#> #   `alignment_scores:align_extra_features` <dbl>, …

# Example 2: Use feature area as the metric
featuretable <- read_featuretable_mzmine(
  featuretable_path,
  intensity = "area"
)

featuretable
#> # A tibble: 20 × 54
#>      UID Feature Sample  Intensity    mz `mz_range:min` `mz_range:max`    rt
#>    <int> <chr>   <chr>       <dbl> <dbl>          <dbl>          <dbl> <dbl>
#>  1     1 1       Sample1     28.9   218.           218.           218. 0.207
#>  2     2 2       Sample1     51.7   161.           161.           161. 0.179
#>  3     3 5       Sample1     18.8   153.           153.           153. 0.194
#>  4     4 8       Sample1     76.0   285.           285.           285. 0.206
#>  5     5 9       Sample1    110.    257.           257.           257. 0.256
#>  6     1 1       Sample2      8.72  218.           218.           218. 0.207
#>  7     2 2       Sample2     56.3   161.           161.           161. 0.179
#>  8     3 5       Sample2     13.2   153.           153.           153. 0.194
#>  9     4 8       Sample2    128.    285.           285.           285. 0.206
#> 10     5 9       Sample2     69.8   257.           257.           257. 0.256
#> 11     1 1       Sample3     24.3   218.           218.           218. 0.207
#> 12     2 2       Sample3     42.5   161.           161.           161. 0.179
#> 13     3 5       Sample3     12.0   153.           153.           153. 0.194
#> 14     4 8       Sample3    288.    285.           285.           285. 0.206
#> 15     5 9       Sample3     25.3   257.           257.           257. 0.256
#> 16     1 1       Sample4     18.6   218.           218.           218. 0.207
#> 17     2 2       Sample4     25.8   161.           161.           161. 0.179
#> 18     3 5       Sample4     14.5   153.           153.           153. 0.194
#> 19     4 8       Sample4     55.1   285.           285.           285. 0.206
#> 20     5 9       Sample4    908     257.           257.           257. 0.256
#> # ℹ 46 more variables: `rt_range:min` <dbl>, `rt_range:max` <dbl>,
#> #   `ion_mobility_range:min` <dbl>, `ion_mobility_range:max` <dbl>, ccs <dbl>,
#> #   ion_mobility_unit <chr>, area <dbl>, height <dbl>,
#> #   `intensity_range:min` <dbl>, `intensity_range:max` <dbl>, charge <dbl>,
#> #   fragment_scans <dbl>, `alignment_scores:rate` <dbl>,
#> #   `alignment_scores:aligned_features_n` <dbl>,
#> #   `alignment_scores:align_extra_features` <dbl>, …

# Example 3: Use the 'mz' column as a Feature label
featuretable <- read_featuretable_mzmine(
  featuretable_path,
  label_col = "mz"
)

featuretable
#> # A tibble: 20 × 54
#>      UID Feature   Sample  Intensity    id `mz_range:min` `mz_range:max`    rt
#>    <int> <chr>     <chr>       <dbl> <dbl>          <dbl>          <dbl> <dbl>
#>  1     1 217.95665 Sample1      809      1           218.           218. 0.207
#>  2     2 161.09601 Sample1      951.     2           161.           161. 0.179
#>  3     3 153.00349 Sample1      352      5           153.           153. 0.194
#>  4     4 285.27863 Sample1     2174      8           285.           285. 0.206
#>  5     5 257.24738 Sample1     2871      9           257.           257. 0.256
#>  6     1 217.95665 Sample2      670      1           218.           218. 0.207
#>  7     2 161.09601 Sample2     1071      2           161.           161. 0.179
#>  8     3 153.00349 Sample2      261      5           153.           153. 0.194
#>  9     4 285.27863 Sample2     1404      8           285.           285. 0.206
#> 10     5 257.24738 Sample2     3278      9           257.           257. 0.256
#> 11     1 217.95665 Sample3      927      1           218.           218. 0.207
#> 12     2 161.09601 Sample3      766      2           161.           161. 0.179
#> 13     3 153.00349 Sample3      501      5           153.           153. 0.194
#> 14     4 285.27863 Sample3     1466      8           285.           285. 0.206
#> 15     5 257.24738 Sample3     1792      9           257.           257. 0.256
#> 16     1 217.95665 Sample4      751      1           218.           218. 0.207
#> 17     2 161.09601 Sample4     1073      2           161.           161. 0.179
#> 18     3 153.00349 Sample4      376      5           153.           153. 0.194
#> 19     4 285.27863 Sample4     2161      8           285.           285. 0.206
#> 20     5 257.24738 Sample4     3074      9           257.           257. 0.256
#> # ℹ 46 more variables: `rt_range:min` <dbl>, `rt_range:max` <dbl>,
#> #   `ion_mobility_range:min` <dbl>, `ion_mobility_range:max` <dbl>, ccs <dbl>,
#> #   ion_mobility_unit <chr>, area <dbl>, height <dbl>,
#> #   `intensity_range:min` <dbl>, `intensity_range:max` <dbl>, charge <dbl>,
#> #   fragment_scans <dbl>, `alignment_scores:rate` <dbl>,
#> #   `alignment_scores:aligned_features_n` <dbl>,
#> #   `alignment_scores:align_extra_features` <dbl>, …
```
