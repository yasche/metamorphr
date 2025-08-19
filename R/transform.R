#' Transforms the intensities by calculating their log
#'
#' @description
#' Log-transforms intensities. The default (base = 10) calculates the log10.
#' This transformation can help reduce heteroscedasticity. See references for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param base Which base should be used for the log-transformation. The default (10) means that log10 values of the intensities are calculated.
#'
#' @return A tibble with log-transformed intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI 10.1186/1471-2164-7-142.
#' }
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   transform_log()
transform_log <- function(data, base = 10) {
  data %>%
    dplyr::mutate(Intensity = log(.data$Intensity, base = .env$base))
}

#' Transforms the intensities by calculating their \emph{n}th root
#'
#' @description
#' Calculates the \emph{n}th root of intensities with x^(1/n). The default (n = 2) calculates the square root.
#' This transformation can help reduce heteroscedasticity. See references for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n The \emph{n}th root to calculate.
#'
#' @return A tibble with power-transformed intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI 10.1186/1471-2164-7-142.
#' }
#'
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   transform_power()
transform_power <- function(data, n = 2) {
  data %>%
    dplyr::mutate(Intensity = .data$Intensity^(1 / .env$n))
}
