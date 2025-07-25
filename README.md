
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 🦋 metamorphr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/yasche/metamorphr/graph/badge.svg)](https://app.codecov.io/gh/yasche/metamorphr)
[![R-CMD-check](https://github.com/yasche/metamorphr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yasche/metamorphr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

metamorphr provides a set of wrapper functions to make working with
metabolomics data more fun. All data is stored in one
[tidy](https://cran.r-project.org/package=tidyr/vignettes/tidy-data.html)
[tibble](https://tibble.tidyverse.org/) which facilitates integration
with [tidyverse](https://www.tidyverse.org/) packages like
[dplyr](https://dplyr.tidyverse.org/) and
[ggplot2](https://ggplot2.tidyverse.org/). Also, functions in metamorphr
are [pipe](https://magrittr.tidyverse.org/index.html)-friendly.

## 📥 Installation

You can install the development version of metamorphr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("yasche/metamorphr")
```

## 🔧 Work in progress

This package is work in progress. Some function names and the way they
work, including their arguments and values, may still change at this
point. To be clear, **the code you write with the current version of
metamorphr might not be compatible with future versions!** You can track
the progress with the figure below.

<figure>
<img src="man/figures/250131_progress.svg"
alt="Current progress as of January 2025" />
<figcaption aria-hidden="true">Current progress as of January
2025</figcaption>
</figure>

## 🧹 Introducing a (somewhat) tidy data format

**TL;DR** Skip to [📑 Examples](#-examples).

A prerequisite for frictionless working with
[tidyverse](https://www.tidyverse.org/) packages is that data is stored
in a
[tidy](https://cran.r-project.org/package=tidyr/vignettes/tidy-data.html)
format. Metabolomics feature tables usually do not fulfill the criteria
for tidy data. They are normally a data frame or a matrix with features
in rows and samples in columns or vice versa. This can lead to
frustration when applying [dplyr](https://dplyr.tidyverse.org/) or
[ggplot2](https://ggplot2.tidyverse.org/) functions. metamorphr provides
convenience functions to read feature tables into a (somewhat) tidy
format and add sample metadata.

Consider this small feature table saved under `my_featuretable.csv` with
a label column, `label`, 2 columns that hold additional feature
metadata, `feature_metadata1` and `feature_metadata2` (*e.g.*, retention
time and *m/z* or some identifier like HMDB or KEGG) and 2 samples,
`sample1` and `sample2`. It further holds the measured intensity (or
area) of 2 features, `feature1` and `feature2` for both samples:

| label    | feature_metadata1 | feature_metadata2 | sample1 | sample2 |
|:---------|:------------------|:------------------|--------:|--------:|
| feature1 | metadata1_1       | metadata2_1       |       1 |       2 |
| feature2 | metadata2_1       | metadata2_2       |       3 |       4 |

The convenience function `read_featuretable()` can read a delimited file
(like csv or tsv), a connection or literal data and transform it to a
(somewhat) tidy tibble:

``` r
library(metamorphr)

my_featuretable <- read_featuretable("my_featuretable.csv", label_col = 1, metadata_cols = c(2, 3))
```

The tibble `my_featuretable` looks like this:

| UID | Feature  | Sample  | Intensity | feature_metadata1 | feature_metadata2 |
|----:|:---------|:--------|----------:|:------------------|:------------------|
|   1 | feature1 | sample1 |         1 | metadata1_1       | metadata2_1       |
|   2 | feature2 | sample1 |         3 | metadata2_1       | metadata2_2       |
|   1 | feature1 | sample2 |         2 | metadata1_1       | metadata2_1       |
|   2 | feature2 | sample2 |         4 | metadata2_1       | metadata2_2       |

As you can see, the function reads the file and performs some
transformation. The first column is a unique identifier (`UID`) for the
features. Column 2 (`Feature`) holds the labels from the original
feature table. The third column (`Sample`) specifies the sample and the
fourth (`Intensity`) holds the measured intensity (or area). Columns 5
and 6 (`feature_metadata1` and `feature_metadata2`) hold additional
feature metadata. These columns are redundant because they can be
predicted from the `UID` column. It is up to the user to drop them or
leave them in. They don’t do any harm and might even be useful for
downstream filtering or plotting.

Sample metadata like (treatment) groups or backgrounds can easily be
added with the 2 functions `create_metadata_skeleton()` and
`join_metadata`. `create_metadata_skeleton()` creates an empty tibble
for you to populate with metadata:

``` r
my_metadata <- my_featuretable %>%
  create_metadata_skeleton()
```

This is the resulting `my_metadata` tibble:

| Sample  | Group | Replicate | Batch | Factor |
|:--------|:------|:----------|:------|-------:|
| sample1 | NA    | NA        | NA    |      1 |
| sample2 | NA    | NA        | NA    |      1 |

By default it has 5 columns and as many rows as there are unique samples
in the underlying featuretable (2 in this case). The first column,
`Sample`, holds the samples. It is already populated with the names of
the samples. The `Group` column is empty and can store grouping
information, for example information about treatments or backgrounds.
The third column, `Replicate` is important if there are multiple
technical replicates, for example if a sample was injected multiple
times or workup was performed with the same starting material multiple
times. Then, the same samples should have the same `Replicate` number
(or character) and `Group` name. `Batch` specifies the batch. It is only
applicable if the sample or measurement was performed in multiple
batches. Finally, `Factor` can hold a numeric factor like dry weight or
protein concentration for sample-specific normalization.

Below is an example for how to populate the metadata table in R.
Alternatively, it can also be saved to disk (e.g., with
`readr::write_csv()`), edited manually and read again (e.g., with
`readr::read_csv()`).

``` r
my_metadata <- my_metadata %>%
  dplyr::mutate(
    Group = c("treatment", "control"),
    Replicate = c(1L, 1L),
    Batch = c(1L, 1L),
    Factor = c(0.976, 1.035)
  )
```

Finally, `my_featuretable` and `my_metadata` can be joined via
`join_metadata()`.

``` r
my_featuretable <- my_featuretable %>%
  join_metadata(my_metadata)
```

| UID | Feature | Sample | Intensity | feature_metadata1 | feature_metadata2 | Group | Replicate | Batch | Factor |
|---:|:---|:---|---:|:---|:---|:---|---:|---:|---:|
| 1 | feature1 | sample1 | 1 | metadata1_1 | metadata2_1 | treatment | 1 | 1 | 0.976 |
| 2 | feature2 | sample1 | 3 | metadata2_1 | metadata2_2 | treatment | 1 | 1 | 0.976 |
| 1 | feature1 | sample2 | 2 | metadata1_1 | metadata2_1 | control | 1 | 1 | 1.035 |
| 2 | feature2 | sample2 | 4 | metadata2_1 | metadata2_2 | control | 1 | 1 | 1.035 |

## 📑 Examples

### 📊 Easy dplyr and ggplot2 integration

Easily plot the distribution of all intensities across samples

``` r
library(dplyr)
library(ggplot2)

toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  ggplot2::ggplot(ggplot2::aes(Sample, Intensity, color = Group)) +
  ggplot2::geom_boxplot() +
  theme_bw()
```

<img src="man/figures/README-example-dplyr-1-1.svg" width="100%" />

… or compare the intensity of specific features across groups

``` r
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  dplyr::filter(Name %in% c("Arachidonic acid", "ADP", "NADPH")) %>%
  dplyr::filter(Group %in% c("control", "treatment")) %>%
  ggplot2::ggplot(ggplot2::aes(Group, Intensity, color = Group)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~Name) +
  ggplot2::theme_bw()
```

<img src="man/figures/README-example-dplyr-2-1.svg" width="100%" />
