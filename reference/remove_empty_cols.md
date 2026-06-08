# Remove empty columns from a tibble or data frame

Remove empty columns (i.e., columns that *only* contain `NA`) from a
tibble or data frame.

## Usage

``` r
remove_empty_cols(data, always_keep = NULL, show_removed_cols = TRUE)
```

## Arguments

- data:

  A tibble or data frame in wide format.

- always_keep:

  Specify columns that should *always* be kept, regardless if they only
  contain `NA` or not. Columns can be specified as a vector with column
  indices (e.g., c(1, 2)), column names as characters (e.g., c("a",
  "b")), as symbols (e.g., c(a, b)), or combinations thereof (e.g., c(1,
  b))

- show_removed_cols:

  If `TRUE` prints a message that shows which columns were removed.

## Value

A tibble or data frame in wide format without empty columns.

## Examples

``` r
# Columns `a` and `d` contains only `NA` and should be removed
na_tibble <- tibble::tibble(
  a = c(NA, NA, NA),
  b = c(1, 2, 3),
  c = c(NA, 2, 3),
  d = c(NA, NA, 3),
  e = c(NA, NA, NA)
)

remove_empty_cols(na_tibble)
#> The following columns were removed: `a`, `e`.
#> # A tibble: 3 × 3
#>       b     c     d
#>   <dbl> <dbl> <dbl>
#> 1     1    NA    NA
#> 2     2     2    NA
#> 3     3     3     3

# Columns `a` and `d` contains only `NA` but `a` should be kept anyways
remove_empty_cols(na_tibble, always_keep = a)
#> The following column was removed: `e`.
#> # A tibble: 3 × 4
#>   a         b     c     d
#>   <lgl> <dbl> <dbl> <dbl>
#> 1 NA        1    NA    NA
#> 2 NA        2     2    NA
#> 3 NA        3     3     3
```
