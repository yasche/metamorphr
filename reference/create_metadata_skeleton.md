# Create a blank metadata skeleton

Takes a tidy tibble created by
[`metamorphr::read_featuretable()`](https://yasche.github.io/metamorphr/reference/read_featuretable.md)
and returns an empty tibble for sample metadata. The tibble can either
be populated directly in R or exported and edited by hand (*e.g.* in
Excel). Metadata are necessary for several downstream functions. **More
columns may be added if necessary**.

## Usage

``` r
create_metadata_skeleton(data)
```

## Arguments

- data:

  A tidy tibble created by
  [`metamorphr::read_featuretable()`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

## Value

An empty tibble structure with the necessary columns for metadata:

- Sample:

  The sample name

- Group:

  To which group does the samples belong? For example a treatment or a
  background. Note that additional columns with additional grouping
  information can be freely added if necessary.

- Replicate:

  If multiple technical replicates exist in the data set, they must have
  the same value for Replicate and the same value for Group so that they
  can be collapsed. Examples for technical replicates are: the same
  sample was injected multiple times or workup was performed multiple
  times with the same starting material. If no technical replicates
  exist, set `Replicate = 1` for all samples.

- Batch:

  The batch in which the samples were prepared or measured. If only one
  batch exists, set `Batch = 1` for all samples.

- Factor:

  A sample-specific factor, for example dry weight or protein content.

## Examples

``` r
featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
metadata <- read_featuretable(featuretable_path, metadata_cols = 2:5) %>%
  create_metadata_skeleton()
```
