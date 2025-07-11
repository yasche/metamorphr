% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputes.R
\name{impute_rf}
\alias{impute_rf}
\title{Impute missing values using random forest}
\usage{
impute_rf(data, random_seed = 1L, ...)
}
\arguments{
\item{data}{A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.}

\item{random_seed}{A seed for the random number generator. Can be an integer or \code{NULL} (in case no particular seed should be used) but for reproducibility reasons it is \strong{strongly advised} to provide an integer.}

\item{...}{Additional parameters passed to \code{\link[missForest]{missForest}}.}
}
\value{
A tibble with imputed missing values.
}
\description{
Basically a wrapper function around \verb{missForest::}\code{\link[missForest]{missForest}}. Imputes missing values using the random forest algorithm.
}
\examples{
toy_metaboscape \%>\%
  impute_rf()
}
\references{
\itemize{
\item \CRANpkg{missForest} on CRAN
\item D. J. Stekhoven, P. Bühlmann, \emph{Bioinformatics} \strong{2012}, \emph{28}, 112–118, DOI 10.1093/bioinformatics/btr597.
}
}
