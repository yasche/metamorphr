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

#' Normalize intensities across samples using standard Quantile Normalization
#'
#' @description
#' This is the standard approach for Quantile Normalization. Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_group}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, <i>Sci Rep</i> <b>2020</b>, <i>10</i>, 15534, DOI <a href = "https://doi.org/10.1038/s41598-020-72664-6">10.1038/s41598-020-72664-6</a>.
#'
#' @examples
#' toy_metaboscape %>%
#'   normalize_quantile_all()
normalize_quantile_all <- function(data)  {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #calculate mean of ties
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie")
}

#' Normalize intensities across samples using grouped Quantile Normalization
#'
#' @description
#' This function performs a Quantile Normalization on each sub-group in the data set. <b>It therefore requires grouping information</b>. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue).
#'
#' Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_all}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#' Note that it is equivalent to the 'Class-specific' normalization in Zhao <i>et al.</i> but has been renamed for internal consistency.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, <i>Sci Rep</i> <b>2020</b>, <i>10</i>, 15534, DOI <a href = "https://doi.org/10.1038/s41598-020-72664-6">10.1038/s41598-020-72664-6</a>.
#'
#' @examples
#' toy_metaboscape %>%
#' #Metadata, including grouping information, must be added before using normalize_quantile_group()
#' join_metadata(toy_metaboscape_metadata) %>%
#' normalize_quantile_group()
normalize_quantile_group <- function(data, group_column) {
  #also called class-specific; named group to make it consistent
  data %>%
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #calculate mean of ties
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie")
}

normalize_quantile_batch <- function() {
  #also called descrete; named batch to make it more consistent
}

normalize_quantile_smooth <- function() {

}

normalize_ref <- function() {

}

normalize_factor <- function() {

}

normalize_cyclic_loess <- function() {
  #also fast_loess?
}

#potential other:
#contrast, cubic_splines, lbs (linear baseline scaling), mstus, non-linear baseline normalization, pqn
