
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 🦋 metamorphr

<!-- badges: start -->
<!-- badges: end -->

metamorphr provides a set of wrapper functions to make working with
metabolomics data more fun. All data is stored in one
[tidy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
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

## 🧹 Introducing a (somewhat) tidy data format

A prerequisite for frictionless working with
[tidyverse](https://www.tidyverse.org/) packages is, that data is stored
in a
[tidy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
format. Metabolomics feature tables usually do not fulfill the criteria
for tidy data. They are normally a data frame or a matrix with features
in rows and samples in columns or vice versa. This can lead to
frustration when applying [dplyr](https://dplyr.tidyverse.org/) or
[ggplot2](https://ggplot2.tidyverse.org/) functions. metamorphr provides
convenience functions to read feature tables into a (somewhat) tidy
format and add sample metadata.

Consider this small feature table saved as `my_featuretable.csv` with a
label column, `label`, 2 columns that hold additional feature metadata,
`feature_metadata1` and `feature_metadata2` (*e.g.*, retention time and
*m/z* or some identifier like HMDB or KEGG) and 2 samples, `sample1` and
`sample2`. It further holds the the measured intensity (or area) of 2
features, `feature1` and `feature2` for both samples:

| label    | feature_metadata1 | feature_metadata2 | sample1 | sample2 |
|:---------|:------------------|:------------------|--------:|--------:|
| feature1 | metadata1_1       | metadata2_1       |       1 |       2 |
| feature2 | metadata2_1       | metadata2_2       |       3 |       4 |

The convenience function `read_featuretable()` can read a delimited file
(like csv or tsv), a connection or literal data and transform it to a
(somewhat) tidy tibble:

``` r
library(metamorphr)

read_featuretable("my_featuretable.csv", label_col = 1, metadata_cols = c(2,3))
```

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

## 📑 Examples

### 📊 Easy dplyr and ggplot2 integration

Easily plot the distribution of all intensities across samples

``` r
library(dplyr)
library(ggplot2)

toy_metaboscape %>%
  dplyr::left_join(toy_metaboscape_metadata, by = "Sample") %>%
  ggplot2::ggplot(ggplot2::aes(Sample, Intensity, color = Group)) +
    ggplot2::geom_boxplot() +
    theme_bw()
```

<img src="man/figures/README-example-dplyr-1-1.svg" width="100%" />

… or compare the intensity of specific features across groups

``` r
toy_metaboscape %>%
  dplyr::left_join(toy_metaboscape_metadata, by = "Sample") %>%
  dplyr::filter(Name %in% c("Arachidonic acid", "ADP", "NADPH")) %>%
  dplyr::filter(Group %in% c("control", "treatment")) %>%
  ggplot2::ggplot(ggplot2::aes(Group, Intensity, color = Group)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~Name) +
    ggplot2::theme_bw()
```

<img src="man/figures/README-example-dplyr-2-1.svg" width="100%" />
