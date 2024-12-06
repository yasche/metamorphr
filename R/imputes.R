#' Impute missing values by replacing them with the lowest observed intensity (global)
#'
#' @description
#' Replace missing intensity values (`NA`) with the lowest observed intensity. This imputation method is very rudimentary.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_global_lowest()
#'
impute_global_lowest <- function(data) {
  data %>%
    dplyr::mutate(LoD = min(.data$Intensity, na.rm = T)) %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ LoD,
                                               .default = .data$Intensity)) %>%
    dplyr::select(-LoD)
}

impute_user_value <- function() {

}

impute_mean <- function() {

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
