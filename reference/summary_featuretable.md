# General information about a feature table and sample-wise summary

Information about a feature table. Prints information to the console
(number of samples, number of features and if applicable number of
groups, replicates and batches) and returns a sample-wise summary as a
list.

## Usage

``` r
summary_featuretable(
  data,
  n_samples_max = 5,
  n_features_max = 5,
  n_groups_max = 5,
  n_batches_max = 5
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- n_samples_max:

  How many Samples should be printed to the console?

- n_features_max:

  How many Features should be printed to the console?

- n_groups_max:

  How many groups should be printed to the console?

- n_batches_max:

  How many Batches should be printed to the console?

## Value

A sample-wise summary as a list.

## Examples

``` r
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  summary_featuretable()
#> 11 Samples: Sample1, Sample2, Sample3, Sample4, Sample5
#> # 6 more samples
#> 
#> # Use the n_samples_max argument to see more
#> 
#> 10 Features: 161.10519 Da 26.98 s, 276.13647 Da 27.28 s, 304.24023 Da 32.86 s, 417.23236 Da 60.08 s, 104.10753 Da 170.31 s
#> # 5 more features
#> 
#> # Use the n_features_max argument to see more
#> 
#> 4 Groups: control, treatment, QC, blank
#> Replicates detected: 1...3
#> 
#> 1 Batches: 1
#> 30 % missing values (NA): 33 out of 110.
#> $Sample1
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   3.000   3.500   4.000   4.143   5.000   5.000       3 
#> 
#> $Sample2
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>    2.00    3.00    4.50    4.75    6.00    9.00       2 
#> 
#> $Sample3
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   1.000   3.500   4.000   4.125   5.250   7.000       2 
#> 
#> $Sample4
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   3.000   3.750   5.500   5.875   8.250   9.000       2 
#> 
#> $Sample5
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   2.000   3.000   7.000   5.444   8.000   9.000       1 
#> 
#> $Sample6
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   4.000   7.250   8.000   7.375   8.250   9.000       2 
#> 
#> $QC1
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   3.000   4.500   5.000   5.375   6.250   9.000       2 
#> 
#> $QC2
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   1.000   2.000   3.000   3.125   3.500   6.000       2 
#> 
#> $QC3
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   1.000   2.750   3.500   4.125   6.000   8.000       2 
#> 
#> $Blank1
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>       4       5       6       6       7       8       7 
#> 
#> $Blank2
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>       5       6       7       7       8       9       8 
#> 
```
