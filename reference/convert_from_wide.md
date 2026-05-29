# Convert a wide feature table to a tidy tibble

Feature tables are usually stored in a "wide" data format where sample
names are stored in columns and features are stored in rows. This
functions transforms those feature tables into a "long" and tidy data
format to use it with functions provided in the `metamorphr` package.
`convert_from_wide` works with tibbles and data frames. To convert a
matrix, see
[`convert_from_matrix`](https://yasche.github.io/metamorphr/reference/convert_from_matrix.md).

## Usage

``` r
convert_from_wide(data, label_col = 1, metadata_cols = NULL)
```

## Arguments

- data:

  A feature table data frame or tibble in wide format. To convert a
  matrix, see
  [`convert_from_matrix`](https://yasche.github.io/metamorphr/reference/convert_from_matrix.md).

- label_col:

  The index or name of the column that will be used to label Features.
  For example an identifier (*e.g.*, KEGG, CAS, HMDB) or a *m/z*-RT
  pair.

- metadata_cols:

  The index/indices or name(s) of column(s) that hold additional feature
  metadata (*e.g.*, retention times, additional identifiers or *m/z*
  values).

## Value

A tidy tibble.

## Examples

``` r
featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
featuretable_wide <- read.csv(featuretable_path)

convert_from_wide(featuretable_wide, metadata_cols = 2:5)
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT   m.z Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1         4  0.45  162. ""         "C7H15…
#>  2     2 276.13647 Da 27.28 s   Sample1         3  0.45  277. "Octyl hy… "C16H2…
#>  3     3 304.24023 Da 32.86 s   Sample1        NA  0.55  305. "Arachido… "C20H3…
#>  4     4 417.23236 Da 60.08 s   Sample1         5  1     418. ""         ""     
#>  5     5 104.10753 Da 170.31 s  Sample1         5  2.84  105. ""         "C5H14…
#>  6     6 105.04259 Da 199.80 s  Sample1         5  3.33  106. ""         "C3H8N…
#>  7     7 237.09204 Da 313.24 s  Sample1        NA  5.22  238. "Ketamine" "C13H1…
#>  8     8 745.09111 Da 382.23 s  Sample1         3  6.37  746. "NADPH"    "C21H3…
#>  9     9 427.02942 Da 424.84 s  Sample1         4  7.08  428. "ADP"      "C10H1…
#> 10    10 1284.34904 Da 498.94 s Sample1        NA  8.32 1285. ""         ""     
#> # ℹ 100 more rows
```
