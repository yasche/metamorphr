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
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ value,
                                               .default = .data$Intensity))
}

#' Impute missing values by replacing them with the Feature mean
#'
#' @description
#' Replace missing intensity values (`NA`) with the Feature mean of non-`NA` values. For example, if a Feature has the measured intensities `NA, 1, NA, 3` in samples 1-4,
#' the intensities after `impute_mean()` would be `2, 1, 2, 3`.
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

impute_median <- function() {

}

impute_min <- function() {

}

impute_lod <- function() {

}

impute_knn <- function() {

}

impute_rf <- function() {

}

impute_svd <- function() {

}

impute_qrilc <- function() {

}
