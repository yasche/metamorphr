#' Transforms the intensities by calculating their log
#'
#' @description
#' Log-transforms intensities. The default (base = 10) calculates the log<sub>10</sub>.
#' This transformation can help reduce heteroscedasticity. See references for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param base Which base should be used for the log-transformation. The default (10) means that log<sub>10</sub> values of the intensities are calculated.
#'
#' @return A tibble with log-transformed intensities.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, <i>BMC Genomics</i> <b>2006</b>, <i>7</i>, 142, DOI <a href = "https://doi.org/10.1186/1471-2164-7-142">10.1186/1471-2164-7-142</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   transform_log()
transform_log <- function(data, base = 10) {
  data %>%
    dplyr::mutate(Intensity = log(.data$Intensity, base = .env$base))
}

#' Transforms the intensities by calculating their <i>n</i>th root
#'
#' @description
#' Calculates the <i>n</i>th root of intensities with x<sup>1/n</sup>. The default (n = 2) calculates the square root.
#' This transformation can help reduce heteroscedasticity. See references for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n The <i>n</i>th root to calculate.
#'
#' @return A tibble with power-transformed intensities.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, <i>BMC Genomics</i> <b>2006</b>, <i>7</i>, 142, DOI <a href = "https://doi.org/10.1186/1471-2164-7-142">10.1186/1471-2164-7-142</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   transform_power()
transform_power <- function(data, n = 2) {
  data %>%
    dplyr::mutate(Intensity = .data$Intensity^(1/.env$n))
}
