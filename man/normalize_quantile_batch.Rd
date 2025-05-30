% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_quantile_batch}
\alias{normalize_quantile_batch}
\title{Normalize intensities across samples using grouped Quantile Normalization with multiple batches}
\usage{
normalize_quantile_batch(
  data,
  group_column = .data$Group,
  batch_column = .data$Batch
)
}
\arguments{
\item{data}{A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.}

\item{group_column}{Which column should be used for grouping? Usually \code{grouping_column = Group}. Uses \code{\link[rlang]{args_data_masking}}.}

\item{batch_column}{Which column contains the batch information? Usually \code{grouping_column = Batch}. Uses \code{\link[rlang]{args_data_masking}}.}
}
\value{
A tibble with intensities normalized across samples.
}
\description{
This function performs a Quantile Normalization on each sub-group and batch in the data set. \if{html}{\out{<b>}}It therefore requires grouping and batch information\if{html}{\out{</b>}}. See
Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
if sub-groups are very different (e.g., when comparing cancer vs. normal tissue).

Other sub-flavors are also available:
\itemize{
\item \code{\link[metamorphr]{normalize_quantile_all}}
\item \code{\link[metamorphr]{normalize_quantile_batch}}
\item \code{\link[metamorphr]{normalize_quantile_smooth}}
}

See References for more information.
Note that it is equivalent to the 'Discrete' normalization in Zhao \if{html}{\out{<i>}}et al.\if{html}{\out{</i>}} but has been renamed for internal consistency.
}
\examples{
toy_metaboscape \%>\%
  # Metadata, including grouping and batch information,
  # must be added before using normalize_quantile_batch()
  join_metadata(toy_metaboscape_metadata) \%>\%
  normalize_quantile_batch(group_column = Group, batch_column = Batch)
}
\references{
Y. Zhao, L. Wong, W. W. B. Goh, \if{html}{\out{<i>}}Sci Rep\if{html}{\out{</i>}} \if{html}{\out{<b>}}2020\if{html}{\out{</b>}}, \if{html}{\out{<i>}}10\if{html}{\out{</i>}}, 15534, DOI \if{html}{\out{<a href = "https://doi.org/10.1038/s41598-020-72664-6">}}10.1038/s41598-020-72664-6\if{html}{\out{</a>}}.
}
