# Normalize intensities across samples using a reference feature

Performs a normalization based on a reference feature, for example an
internal standard. Divides the Intensities of all features by the
Intensity of the reference feature in that sample and multiplies them
with a constant value, making the Intensity of the reference feature the
same in each sample.

## Usage

``` r
normalize_ref(
  data,
  reference_feature,
  identifier_column,
  reference_feature_intensity = 1
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- reference_feature:

  An identifier for the reference feature. Must be unique. It is
  recommended to use the UID.

- identifier_column:

  The column in which to look for the reference feature. It is
  recommended to use `identifier_column = UID`

- reference_feature_intensity:

  Either a constant value with which the intensity of each feature is
  multiplied or a function (e.g., mean, median, min, max). If a function
  is provided, it will use that function on the Intensities of the
  reference feature in all samples before normalization and multiply the
  intensity of each feature with that value after dividing by the
  Intensity of the reference feature. For example, if
  `reference_feature_intensity = mean`, it calculates the mean of the
  Intensities of the reference features across samples before
  normalization. It then divides the Intensity of each feature by the
  Intensity of the reference feature in that sample. Finally, it
  multiplies each Intensity with the mean of the Intensities of the
  reference features prior to normalization.

## Value

A tibble with intensities normalized across samples.

## Examples

``` r
# Divide by the reference feature and make its Intensity 1000 in each sample
toy_metaboscape %>%
  impute_lod() %>%
  normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = 1000)
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1    1333.   0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1    1000    0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1      66.7  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1    1667.   1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1    1667.   2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1    1667.   3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1     467.   5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1    1000    6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1    1333.   7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1     133.   8.32 1285. NA         NA     
#> # ℹ 100 more rows

# Divide by the reference feature and make its Intensity the mean of intensities
# of the reference features before normalization
toy_metaboscape %>%
  impute_lod() %>%
  normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = mean)
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1     6.42   0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1     4.82   0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1     0.321  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1     8.03   1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1     8.03   2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1     8.03   3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1     2.25   5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1     4.82   6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1     6.42   7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1     0.642  8.32 1285. NA         NA     
#> # ℹ 100 more rows
```
