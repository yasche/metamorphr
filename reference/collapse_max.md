# Collapse intensities of technical replicates by calculating their maximum

Calculates the minimum of the intensity of technical replicates (e.g.,
if the same sample was injected multiple times or if multiple workups
have been performed on the same starting material). The function assigns
new sample names by joining either group and replicate name, or if a
batch column is specified group, replicate and batch together with a
specified separator. Due to the nature of the function, sample and
feature metadata columns will be dropped unless they are specified with
the according arguments.

## Usage

``` r
collapse_max(
  data,
  group_column = .data$Group,
  replicate_column = .data$Replicate,
  batch_column = .data$Batch,
  feature_metadata_cols = "Feature",
  sample_metadata_cols = NULL,
  separator = "_"
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- group_column:

  Which column should be used for grouping? Usually
  `grouping_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- replicate_column:

  Which column contains replicate information? Usually
  `replicate_column = Replicate`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- batch_column:

  Which column contains batch information? If all samples belong to the
  same batch (i.e., they all have the same batch identifier in the
  `batch_column`) it will have no effect on the calculation. Usually
  `batch_column = Batch`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

- feature_metadata_cols:

  A character or character vector containing the names of the feature
  metadata columns. They are usually created when reading the feature
  table with
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).
  Feature metadata columns not specified here will be dropped.

- sample_metadata_cols:

  A character or character vector containing the names of the sample
  metadata columns. They are usually created when joining the metadata
  with
  [`join_metadata`](https://yasche.github.io/metamorphr/reference/join_metadata.md).
  Sample metadata columns not specified here will be dropped, except for
  `group_column`, `replicate_column` and `batch_column` if specified.

- separator:

  Separator used for joining group and replicate, or group, batch and
  replicate together to create the new sample names. The new sample
  names will be Group name, separator, Batch name, separator, Replicate
  name, or Group name, separator, Replicate name, in case all samples
  belong to the same batch (i.e., they all have the same batch
  identifier in the `batch_column`).

## Value

A tibble with intensities of technical replicates collapsed.

## Examples

``` r
# uses a slightly modified version of toy_metaboscape_metadata
collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
collapse_toy_metaboscape_metadata$Replicate <- 1

toy_metaboscape %>%
  join_metadata(collapse_toy_metaboscape_metadata) %>%
  impute_lod() %>%
  collapse_max(group_column = Group, replicate_column = Replicate)
#> # A tibble: 40 × 7
#>      UID Feature              Sample      Intensity Group     Replicate Batch
#>    <int> <chr>                <chr>           <dbl> <chr>         <dbl> <int>
#>  1     1 161.10519 Da 26.98 s QC_1              7   QC                1     1
#>  2     1 161.10519 Da 26.98 s blank_1           0.6 blank             1     1
#>  3     1 161.10519 Da 26.98 s control_1         4   control           1     1
#>  4     1 161.10519 Da 26.98 s treatment_1       9   treatment         1     1
#>  5     2 276.13647 Da 27.28 s QC_1              9   QC                1     1
#>  6     2 276.13647 Da 27.28 s blank_1           9   blank             1     1
#>  7     2 276.13647 Da 27.28 s control_1         6   control           1     1
#>  8     2 276.13647 Da 27.28 s treatment_1       4   treatment         1     1
#>  9     3 304.24023 Da 32.86 s QC_1              6   QC                1     1
#> 10     3 304.24023 Da 32.86 s blank_1           0.2 blank             1     1
#> # ℹ 30 more rows
```
