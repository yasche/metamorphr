# Sample metadata for the fictional dataset `toy_metaboscape`

Data was generated with
[`metamorphr::create_metadata_skeleton()`](https://yasche.github.io/metamorphr/reference/create_metadata_skeleton.md)
and can be reproduced with
`metamorphr::toy_metaboscape %>% create_metadata_skeleton()`.'

## Usage

``` r
toy_metaboscape_metadata
```

## Format

### `toy_metaboscape_metadata`

A data frame with 11 rows and 5 columns:

- Sample:

  The sample name

- Group:

  To which group does the samples belong? For example a treatment or a
  background. Note that additional columns with additional grouping
  information can be freely added if necessary.

- Replicate:

  The replicate.

- Batch:

  The batch in which the samples were prepared or measured.

- Factor:

  A sample-specific factor, for example dry weight or protein content.

## Source

This data set contains fictional data!
