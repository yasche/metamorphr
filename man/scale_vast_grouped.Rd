% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale.R
\name{scale_vast_grouped}
\alias{scale_vast_grouped}
\title{Scale intensities of features using grouped vast scaling}
\usage{
scale_vast_grouped(data, group_column = .data$Group)
}
\arguments{
\item{data}{A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.}

\item{group_column}{Which column should be used for grouping? Usually \code{grouping_column = Group}. Uses \code{\link[rlang]{args_data_masking}}.}
}
\value{
A tibble with vast scaled intensities.
}
\description{
A variation of \code{\link[metamorphr]{scale_vast}} but uses a group-specific coefficient of variation and therefore requires group information. See \code{\link[metamorphr]{scale_vast}} and the References section for more information.
}
\examples{
toy_metaboscape \%>\%
  join_metadata(toy_metaboscape_metadata) \%>\%
  scale_vast_grouped()
}
\references{
\itemize{
\item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
}
}
