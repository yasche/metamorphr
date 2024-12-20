#' Impute missing values by replacing them with the lowest observed intensity (global)
#'
#' @description
#' Replace missing intensity values (`NA`) with the lowest observed intensity.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_global_lowest()

impute_global_lowest <- function(data) {
  data %>%
    dplyr::mutate(LoD = min(.data$Intensity, na.rm = T)) %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .data$LoD,
                                               .default = .data$Intensity)) %>%
    dplyr::select(-"LoD")
}



#' Impute missing values by replacing them with a user-provided value
#'
#' @description
#' Replace missing intensity values (`NA`) with a user-provided value (e.g., 1).
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#' @param value Numeric that replaces missing values
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_user_value(value = 1)

impute_user_value <- function(data, value) {
  data %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .env$value,
                                               .default = .data$Intensity))
}

#' Impute missing values by replacing them with the Feature mean
#'
#' @description
#' Replace missing intensity values (`NA`) with the Feature mean of non-`NA` values. For example, if a Feature has the measured intensities `NA, 1, NA, 3, 2` in samples 1-4,
#' the intensities after `impute_mean()` would be `2, 1, 2, 3, 2`.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_mean()

impute_mean <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(LoD = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .data$LoD,
                                               .default = .data$Intensity)) %>%
    dplyr::select(-"LoD")
}

#' Impute missing values by replacing them with the Feature median
#'
#' @description
#' Replace missing intensity values (`NA`) with the Feature median of non-`NA` values. For example, if a Feature has the measured intensities `NA, 1, NA, 3, 2` in samples 1-4,
#' the intensities after `impute_median()` would be `2, 1, 2, 3, 2`.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_median()

impute_median <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(LoD = stats::median(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .data$LoD,
                                               .default = .data$Intensity)) %>%
    dplyr::select(-"LoD")
}


#' Impute missing values by replacing them with the Feature minimum
#'
#' @description
#' Replace missing intensity values (`NA`) with the Feature minimum of non-`NA` values.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_min()
impute_min <- function(data) {
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(LoD = min(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .data$LoD,
                                               .default = .data$Intensity)) %>%
    dplyr::select(-"LoD")
}

#' Impute missing values by replacing them with the Feature 'Limit of Detection'
#'
#' @description
#' Replace missing intensity values (`NA`) by what is assumed to be the detector limit of detection (LoD).
#' It is estimated by dividing the Feature minimum by the provided denominator, usually 5. See the References section for more information.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#' @param div_by A numeric value that specifies by which number the Feature minimum will be divided
#'
#' @return A tibble with imputed missing values.
#' @references <a href="https://omicsforum.ca/t/how-to-deal-with-missing-values/75">LoD on OmicsForum</a>
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod()
impute_lod <- function(data, div_by = 5) {
  #https://omicsforum.ca/t/how-to-deal-with-missing-values/75
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(LoD = min(.data$Intensity, na.rm = T) / .env$div_by) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .data$LoD,
                                               .default = .data$Intensity)) %>%
    dplyr::select(-"LoD")
}

impute_knn <- function() {

}

impute_rf <- function() {

}

impute_svd <- function() {

}

impute_qrilc <- function() {

}
