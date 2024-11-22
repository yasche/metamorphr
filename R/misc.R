#' Join a featuretable and sample metadata
#'
#' @description
#' Joins a featuretable and associated sample metadata. Basically a wrapper around \code{\link[dplyr]{left_join}} where `by = "Sample"`.
#'
#'
#' @param data A feature table created with \code{\link[metamorphr]{read_featuretable}}
#' @param metadata Sample metadata created with \code{\link[metamorphr]{create_metadata_skeleton}}
#'
#' @return A tibble with added sample metadata.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata)
join_metadata <- function(data, metadata) {
  dplyr::left_join(data, metadata, by = "Sample")
}



featuretable_summary <- function(data) {

}
