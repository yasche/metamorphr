#' Transforms the intensities by calculating their log
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param base Which base should be used for the log-transformation. The default (10) means that log<sub>10</sub> values of the intensities are calculated.
#'
#' @return A tibble with log-transformed intensities.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   transform_log()
transform_log <- function(data, base = 10) {
  data %>%
    dplyr::mutate(Intensity = log(.data$Intensity, base = base))
}
