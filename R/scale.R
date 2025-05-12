#' Center intensities of features around zero
#'
#' @description
#' Centers the intensities of all features around zero using
#'
#' \deqn{\widetilde{x}_{ij}=x_{ij}-\overline{x}_{i}}
#'
#' where \eqn{\widetilde{x}_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} after scaling,
#' \eqn{x_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} before scaling and \eqn{\overline{x}_{i}} is the mean of intensities of feature \eqn{i} across all samples.
#' In other words, it subtracts the mean intensity of a feature across samples from the intensities of that feature in each sample.
#' For more information, see the reference section.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities scaled around zero.
#' @export
#'
#' @references For further information, see
#' <ul>
#' R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, <i>BMC Genomics</i> <b>2006</b>, <i>7</i>, 142, DOI <a href = "https://doi.org/10.1186/1471-2164-7-142">10.1186/1471-2164-7-142</a>.
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   scale_center()
scale_center <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = .data$Intensity - mean(.data$Intensity, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

#' Scale intensities of features using autoscale
#'
#' @description
#' Scales the intensities of all features using
#'
#' \deqn{\widetilde{x}_{ij}=\frac{x_{ij}-\overline{x}_{i}}{s_i}}
#'
#' where \eqn{\widetilde{x}_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} after scaling,
#' \eqn{x_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} before scaling, \eqn{\overline{x}_{i}} is the mean of intensities of feature \eqn{i} across all samples
#' and \eqn{{s_i}} is the standard deviation of intensities of feature \eqn{i} across all samples
#' In other words, it subtracts the mean intensity of a feature across samples from the intensities of that feature in each sample and divides by the standard deviation of that feature.
#' For more information, see the reference section.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with autoscaled intensities.
#' @export
#'
#' @references For further information, see
#' <ul>
#' R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, <i>BMC Genomics</i> <b>2006</b>, <i>7</i>, 142, DOI <a href = "https://doi.org/10.1186/1471-2164-7-142">10.1186/1471-2164-7-142</a>.
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   scale_auto()
scale_auto <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = (.data$Intensity - mean(.data$Intensity, na.rm = TRUE)) / stats::sd(.data$Intensity, na.rm = TRUE)) %>%
    dplyr::ungroup()
}

#' Scale intensities of features using range scaling
#'
#' @description
#' Scales the intensities of all features using
#'
#' \deqn{\widetilde{x}_{ij}=\frac{x_{ij}-\overline{x}_{i}}{x_{i,max}-x_{i,min}}}
#'
#' where \eqn{\widetilde{x}_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} after scaling,
#' \eqn{x_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} before scaling, \eqn{\overline{x}_{i}} is the mean of intensities of feature \eqn{i} across all samples,
#' \eqn{x_{i,max}} is the maximum intensity of feature \eqn{i} across all samples and \eqn{x_{i,min}} is the minimum intensity of feature \eqn{i} across all samples.
#' In other words, it subtracts the mean intensity of a feature across samples from the intensities of that feature in each sample and divides by the range of that feature.
#' For more information, see the reference section.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with range scaled intensities.
#' @export
#'
#' @references For further information, see
#' <ul>
#' R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, <i>BMC Genomics</i> <b>2006</b>, <i>7</i>, 142, DOI <a href = "https://doi.org/10.1186/1471-2164-7-142">10.1186/1471-2164-7-142</a>.
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   scale_range()
scale_range <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = (.data$Intensity - mean(.data$Intensity, na.rm = TRUE)) / (max(.data$Intensity, na.rm = TRUE) - min(.data$Intensity, na.rm = TRUE))) %>%
    dplyr::ungroup()
}

#' Scale intensities of features using Pareto scaling
#'
#' @description
#' Scales the intensities of all features using
#'
#' \deqn{\widetilde{x}_{ij}=\frac{x_{ij}-\overline{x}_{i}}{\sqrt{s_i}}}
#'
#' where \eqn{\widetilde{x}_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} after scaling,
#' \eqn{x_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} before scaling, \eqn{\overline{x}_{i}} is the mean of intensities of feature \eqn{i} across all samples
#' and \eqn{{\sqrt{s_i}}} is the square root of the standard deviation of intensities of feature \eqn{i} across all samples
#' In other words, it subtracts the mean intensity of a feature across samples from the intensities of that feature in each sample and divides by the sqaure root of the standard deviation of that feature.
#' For more information, see the reference section.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with autoscaled intensities.
#' @export
#'
#' @references For further information, see
#' <ul>
#' R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, <i>BMC Genomics</i> <b>2006</b>, <i>7</i>, 142, DOI <a href = "https://doi.org/10.1186/1471-2164-7-142">10.1186/1471-2164-7-142</a>.
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   scale_pareto()
scale_pareto <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = (.data$Intensity - mean(.data$Intensity, na.rm = TRUE)) / sqrt(stats::sd(.data$Intensity, na.rm = TRUE))) %>%
    dplyr::ungroup()
}
