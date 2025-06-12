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
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' }
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
#' and \eqn{{s_i}} is the standard deviation of intensities of feature \eqn{i} across all samples.
#' In other words, it subtracts the mean intensity of a feature across samples from the intensities of that feature in each sample and divides by the standard deviation of that feature.
#' For more information, see the reference section.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with autoscaled intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' }
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
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' }
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
#' and \eqn{{\sqrt{s_i}}} is the square root of the standard deviation of intensities of feature \eqn{i} across all samples.
#' In other words, it subtracts the mean intensity of a feature across samples from the intensities of that feature in each sample and divides by the square root of the standard deviation of that feature.
#' For more information, see the reference section.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with autoscaled intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' }
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

#' Scale intensities of features using vast scaling
#'
#' @description
#' Scales the intensities of all features using
#'
#' \deqn{\widetilde{x}_{ij}=\frac{x_{ij}-\overline{x}_{i}}{s_i}\cdot \frac{\overline{x}_{i}}{s_i}}
#'
#' where \eqn{\widetilde{x}_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} after scaling,
#' \eqn{x_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} before scaling, \eqn{\overline{x}_{i}} is the mean of intensities of feature \eqn{i} across all samples
#' and \eqn{{s_i}} is the standard deviation of intensities of feature \eqn{i} across all samples. Note that \eqn{\frac{\overline{x}_{i}}{s_i} = \frac{{1}}{CV}} where CV is the coefficient of variation across all samples.
#' \code{\link[metamorphr]{scale_vast_grouped}} is a variation of this function that uses a group-specific coefficient of variation.
#' In other words, it performs autoscaling (\code{\link[metamorphr]{scale_auto}}) and divides by the coefficient of variation, thereby reducing the importance of features with a poor reproducibility.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with vast scaled intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' \item J. Sun, Y. Xia, \emph{Genes & Diseases} \strong{2024}, \emph{11}, 100979, DOI \href{https://doi.org/10.1016/j.gendis.2023.04.018}{10.1016/j.gendis.2023.04.018}.
#' }
#'
#' @examples
#' toy_metaboscape %>%
#'   scale_vast()
scale_vast <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = ((.data$Intensity - mean(.data$Intensity, na.rm = TRUE)) / stats::sd(.data$Intensity, na.rm = TRUE)) * (mean(.data$Intensity, na.rm = TRUE) / stats::sd(.data$Intensity, na.rm = TRUE))) %>%
    dplyr::ungroup()
}

#' Scale intensities of features using grouped vast scaling
#'
#' @description
#' A variation of \code{\link[metamorphr]{scale_vast}} but uses a group-specific coefficient of variation and therefore requires group information. See \code{\link[metamorphr]{scale_vast}} and the References section for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with vast scaled intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' }
#'
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   scale_vast_grouped()
scale_vast_grouped <- function(data, group_column = .data$Group) {
  data %>%
    dplyr::group_by({{ group_column }}, .data$UID) %>%
    dplyr::mutate(
      Group_mean_Int = mean(.data$Intensity, na.rm = TRUE),
      Group_sd_Int = stats::sd(.data$Intensity, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = ((.data$Intensity - mean(.data$Intensity, na.rm = TRUE)) / stats::sd(.data$Intensity, na.rm = TRUE)) * (.data$Group_mean_Int / .data$Group_sd_Int)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Group_mean_Int", -"Group_sd_Int")
}


#' Scale intensities of features using level scaling
#'
#' @description
#' Scales the intensities of all features using
#'
#' \deqn{\widetilde{x}_{ij}=\frac{x_{ij}-\overline{x}_{i}}{\overline{x}_{i}}}
#'
#' where \eqn{\widetilde{x}_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} after scaling,
#' \eqn{x_{ij}} is the intensity of sample \eqn{j}, feature \eqn{i} before scaling and \eqn{\overline{x}_{i}} is the mean of intensities of feature \eqn{i} across all samples
#'
#' In other words, it performs centering (\code{\link[metamorphr]{scale_center}}) and divides by the feature mean, thereby focusing on the relative intensity.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with level scaled intensities.
#' @export
#'
#' @references \itemize{
#' \item R. A. Van Den Berg, H. C. Hoefsloot, J. A. Westerhuis, A. K. Smilde, M. J. Van Der Werf, \emph{BMC Genomics} \strong{2006}, \emph{7}, 142, DOI \href{https://doi.org/10.1186/1471-2164-7-142}{10.1186/1471-2164-7-142}.
#' }
#'
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   scale_level()
scale_level <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(Intensity = (.data$Intensity - mean(.data$Intensity)) / mean(.data$Intensity)) %>%
    dplyr::ungroup()
}
