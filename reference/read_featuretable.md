# Read a feature table into a tidy tibble

Basically a wrapper around
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
but performs some initial tidying operations such as `gather()`
rearranging columns. The `label_col` will be renamed to *Feature*.

## Usage

``` r
read_featuretable(file, delim = ",", label_col = 1, metadata_cols = NULL, ...)
```

## Arguments

- file:

  A path to a file but can also be a connection or literal data.

- delim:

  The field separator or delimiter. For example "," in csv files.

- label_col:

  The index or name of the column that will be used to label Features.
  For example an identifier (*e.g.*, KEGG, CAS, HMDB) or a *m/z*-RT
  pair.

- metadata_cols:

  The index/indices or name(s) of column(s) that hold additional feature
  metadata (*e.g.*, retention times, additional identifiers or *m/z*
  values).

- ...:

  Additional arguments passed on to
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)

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
# Read a toy dataset in the format produced with Bruker MetaboScape (Version 2021).
featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")

# Example 1: Provide indices for metadata_cols
featuretable <- read_featuretable(featuretable_path, metadata_cols = 2:5)

featuretable
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1         4  0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1         3  0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1        NA  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1         5  1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1         5  2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1         5  3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1        NA  5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1         3  6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1         4  7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1        NA  8.32 1285. NA         NA     
#> # ℹ 100 more rows

# Example 2: Provide a name for label_col and indices for metadata_cols
featuretable <- read_featuretable(
  featuretable_path,
  label_col = "m/z",
  metadata_cols = c(1, 2, 4, 5)
)

featuretable
#> # A tibble: 110 × 8
#>      UID Feature    Sample  Intensity `Bucket label`            RT Name  Formula
#>    <int> <chr>      <chr>       <dbl> <chr>                  <dbl> <chr> <chr>  
#>  1     1 162.11302  Sample1         4 161.10519 Da 26.98 s    0.45 NA    C7H15N…
#>  2     2 277.1443   Sample1         3 276.13647 Da 27.28 s    0.45 Octy… C16H22…
#>  3     3 305.24806  Sample1        NA 304.24023 Da 32.86 s    0.55 Arac… C20H32…
#>  4     4 418.24019  Sample1         5 417.23236 Da 60.08 s    1    NA    NA     
#>  5     5 105.11536  Sample1         5 104.10753 Da 170.31 s   2.84 NA    C5H14NO
#>  6     6 106.05042  Sample1         5 105.04259 Da 199.80 s   3.33 NA    C3H8NO3
#>  7     7 238.09987  Sample1        NA 237.09204 Da 313.24 s   5.22 Keta… C13H16…
#>  8     8 746.09894  Sample1         3 745.09111 Da 382.23 s   6.37 NADPH C21H30…
#>  9     9 428.03725  Sample1         4 427.02942 Da 424.84 s   7.08 ADP   C10H15…
#> 10    10 1285.35687 Sample1        NA 1284.34904 Da 498.94 s  8.32 NA    NA     
#> # ℹ 100 more rows

# Example 3: Provide names for both, label_col and metadata_cols
featuretable <- read_featuretable(
  featuretable_path,
  label_col = "m/z",
  metadata_cols = c("Bucket label", "RT", "Name", "Formula")
)

featuretable
#> # A tibble: 110 × 8
#>      UID Feature    Sample  Intensity `Bucket label`            RT Name  Formula
#>    <int> <chr>      <chr>       <dbl> <chr>                  <dbl> <chr> <chr>  
#>  1     1 162.11302  Sample1         4 161.10519 Da 26.98 s    0.45 NA    C7H15N…
#>  2     2 277.1443   Sample1         3 276.13647 Da 27.28 s    0.45 Octy… C16H22…
#>  3     3 305.24806  Sample1        NA 304.24023 Da 32.86 s    0.55 Arac… C20H32…
#>  4     4 418.24019  Sample1         5 417.23236 Da 60.08 s    1    NA    NA     
#>  5     5 105.11536  Sample1         5 104.10753 Da 170.31 s   2.84 NA    C5H14NO
#>  6     6 106.05042  Sample1         5 105.04259 Da 199.80 s   3.33 NA    C3H8NO3
#>  7     7 238.09987  Sample1        NA 237.09204 Da 313.24 s   5.22 Keta… C13H16…
#>  8     8 746.09894  Sample1         3 745.09111 Da 382.23 s   6.37 NADPH C21H30…
#>  9     9 428.03725  Sample1         4 427.02942 Da 424.84 s   7.08 ADP   C10H15…
#> 10    10 1285.35687 Sample1        NA 1284.34904 Da 498.94 s  8.32 NA    NA     
#> # ℹ 100 more rows
```
