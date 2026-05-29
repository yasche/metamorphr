# Convert a wide matrix to a tidy tibble

This functions transforms a matrix holding a wide feature table into a
"long" and tidy tibble to use it with functions provided in the
`metamorphr` package. `convert_from_matrix` works with objects of class
`matrix.` To convert a data frame or tibble, see
[`convert_from_wide`](https://yasche.github.io/metamorphr/reference/convert_from_wide.md).

## Usage

``` r
convert_from_matrix(data, samples_in_cols = TRUE)
```

## Arguments

- data:

  A feature table matrix in wide format. To convert a wide data frame,
  see
  [`convert_from_wide`](https://yasche.github.io/metamorphr/reference/convert_from_wide.md).

- samples_in_cols:

  `TRUE` if samples are in columns and features in rows, `FALSE` if it
  is reversed. See examples for more information.

## Value

A tidy tibble.

## Examples

``` r
# Using a small fictional data set
dataset <- matrix(1:9, ncol = 3)
colnames(dataset) <- paste0("sample", 1:3)
rownames(dataset) <- paste0("feature", 1:3)


# Example 1: Samples in columns
dataset
#>          sample1 sample2 sample3
#> feature1       1       4       7
#> feature2       2       5       8
#> feature3       3       6       9
convert_from_matrix(dataset)
#> # A tibble: 9 × 4
#>     UID Feature  Sample  Intensity
#>   <int> <chr>    <chr>       <dbl>
#> 1     1 feature1 sample1         1
#> 2     2 feature2 sample1         2
#> 3     3 feature3 sample1         3
#> 4     1 feature1 sample2         4
#> 5     2 feature2 sample2         5
#> 6     3 feature3 sample2         6
#> 7     1 feature1 sample3         7
#> 8     2 feature2 sample3         8
#> 9     3 feature3 sample3         9

# Example 2: Samples in rows
dataset_transposed <- t(dataset)
dataset_transposed
#>         feature1 feature2 feature3
#> sample1        1        2        3
#> sample2        4        5        6
#> sample3        7        8        9

convert_from_matrix(dataset, samples_in_cols = FALSE)
#> # A tibble: 9 × 4
#>     UID Feature Sample   Intensity
#>   <int> <chr>   <chr>        <dbl>
#> 1     1 sample1 feature1         1
#> 2     2 sample2 feature1         4
#> 3     3 sample3 feature1         7
#> 4     1 sample1 feature2         2
#> 5     2 sample2 feature2         5
#> 6     3 sample3 feature2         8
#> 7     1 sample1 feature3         3
#> 8     2 sample2 feature3         6
#> 9     3 sample3 feature3         9
```
