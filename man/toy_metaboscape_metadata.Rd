% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{toy_metaboscape_metadata}
\alias{toy_metaboscape_metadata}
\title{Sample metadata for the fictional dataset \code{toy_metaboscape}}
\format{
\subsection{\code{toy_metaboscape_metadata}}{

A data frame with 11 rows and 5 columns:
\describe{
\item{Sample}{The sample name}
\item{Group}{To which group does the samples belong? For example a treatment or a background. Note that additional columns with additional grouping information can be freely added if necessary.}
\item{Replicate}{The replicate.}
\item{Batch}{The batch in which the samples were prepared or measured.}
\item{Factor}{A sample-specific factor, for example dry weight or protein content.}
...
}
}
}
\source{
This data set contains fictional data!
}
\usage{
toy_metaboscape_metadata
}
\description{
Data was generated with \code{metamorphr::create_metadata_skeleton()} and can be reproduced with
\code{metamorphr::toy_metaboscape \%>\% create_metadata_skeleton()}.'
}
\keyword{datasets}
