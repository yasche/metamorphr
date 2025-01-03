#' Normalize intensities across samples by dividing by the sample median
#'
#' @description
#' Normalize across samples by dividing feature intensities by the sample median, making the median 1 in all samples.
#' See References for more information.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references T. Ramirez, A. Strigun, A. Verlohner, H.-A. Huener, E. Peter, M. Herold, N. Bordag, W. Mellert, T. Walk, M. Spitzer, X. Jiang, S. Sperber, T. Hofmann, T. Hartung, H. Kamp, B. Van Ravenzwaay, <i>Arch Toxicol</i> <b>2018</b>, <i>92</i>, 893–906, DOI <a href = "https://doi.org/10.1007/s00204-017-2079-6">10.1007/s00204-017-2079-6</a>.
#'
#' @examples
#' toy_metaboscape %>%
#'   normalize_median()
normalize_median <- function(data) {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Intensity = .data$Intensity / stats::median(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup()
}

#' Normalize intensities across samples by dividing by the sample sum
#'
#' @description
#' Normalize across samples by dividing feature intensities by the sum of all intensities in a sample, making the sum 1 in all samples.
#'
#' <b>Important Note</b><br>
#' Intensities of individual features will be very small after this normalization approach. It is therefore advised to multiply all intensities with a fixed number (e.g., 1000) after normalization.
#' See <a href = "https://omicsforum.ca/t/sum-normalization-needs-clarification-or-potentially-has-an-issue/3244">this discussion on OMICSForum.ca</a> and the examples below
#' for further information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @examples
#' #Example 1: Normalization only
#' toy_metaboscape %>%
#'   normalize_sum()
#'
#' #Example 2: Multiply with 1000 after normalization
#' toy_metaboscape %>%
#'   normalize_sum() %>%
#'   dplyr::mutate(Intensity = .data$Intensity * 1000)
normalize_sum <- function(data) {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Intensity = .data$Intensity / sum(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup()
}

normalize_quantile_all <- function()  {

}

normalize_quantile_group <- function() {
  #also called class-specific; named group to make it consistent
}

normalize_quantile_batch <- function() {
  #also called descrete; named batch to make it more consistent
}

normalize_quantile_smooth <- function() {

}

normalize_reference_feature <- function() {

}

normalize_sample_specific <- function() {

}

normalize_cyclic_loess <- function() {
  #also fast_loess?
}

#potential other:
#contrast, cubic_splines, lbs (linear baseline scaling), mstus, non-linear baseline normalization, pqn
