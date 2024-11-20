#' Filter Features based on the absolute number or fraction of samples it was not found in
#'
#' @description
#' One of several filter functions. Can be used to filter features based on the number or fraction of samples they are missing in.
#' This is usually one of the first steps in metabolomics data analysis and often already performed when the feature table is first created.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#' @param max_missing In how many samples can a Feature be missing? If `fraction == TRUE`, a value between 0 and 1 (_e.g._, 0.5 if a Feature can be missing in half the samples and not to be filtered out). If `fraction == FALSE` the absolute maximum number of samples (_e.g._, 5 if a specific Feature can be missing in up to 5 samples for it not to be filtered out).
#' @param fraction Either `TRUE` or `FALSE`. Should `max_missing` be the absolute number of samples or a fraction?
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' #Example 1: A feature can be missing in up to 50 % of the samples
#' toy_metaboscape %>%
#'   filter_global_mv(max_missing = 0.5)
#'
#' #Example 2: A feature can be missing in up to 3 samples
#' toy_metaboscape %>%
#'   filter_global_mv(max_missing = 3, fraction = FALSE)
filter_global_mv <- function(data, max_missing, fraction = TRUE) {
  data <- data %>%
    dplyr::add_count(.data$UID, wt = is.na(.data$Intensity), name = "n_na") %>%
    dplyr::group_by(.data$UID)

  if (fraction == TRUE) {
    if (max_missing > 1) {
      stop("Argument max_missing must be <= 1 if argument fraction is TRUE.")
    }

     data %>%
       dplyr::mutate(perc_na = .data$n_na / dplyr::n()) %>%
       dplyr::filter(.data$perc_na <= max_missing) %>%
       dplyr::ungroup() %>%
       dplyr::select(-"n_na", -"perc_na")

  } else {

    data %>%
      dplyr::filter(.data$n_na <= max_missing) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"n_na")
  }


}

#' Group-based feature filtering
#'
#' @description
#' One of several filter functions. Similar to `filter_global_mv()` it filters features that are missing in a specified number of samples.
#' The key difference is, that `filter_grouped_mv()` takes groups into consideration and therefore relies on sample metadata.
#' For example, if `fraction = TRUE` and `max_missing = 0.25`, a feature must be found in at least 75 % of the samples of at least one group.
#' It is very similar to the _Filter features by occurrences in groups_ option in Bruker MetaboScape.
#'
#' @param data abc
#' @param grouping_column def
#' @param max_missing ghi
#' @param fraction jkl
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples #abc
filter_grouped_mv <- function(data, grouping_column, max_missing, fraction = TRUE) {
  # using injection: https://rlang.r-lib.org/reference/topic-inject.html

  data <- data %>%
    dplyr::add_count(.data$UID, {{ grouping_column }}, wt = is.na(.data$Intensity), name = "n_na")

  if (fraction == TRUE) {
    if (max_missing > 1) {
      stop("Argument max_missing must be <= 1 if argument fraction is TRUE.")
    }

    data %>%
      dplyr::group_by(.data$UID, {{ grouping_column }}) %>%
      dplyr::mutate(perc_na = .data$n_na / dplyr::n()) %>%
      #print()
      dplyr::ungroup() %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(min_perc_na = min(.data$perc_na)) %>%
      dplyr::filter(.data$min_perc_na <= max_missing) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"n_na", -"perc_na", -"min_perc_na")

  } else {

    data %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(min_na = min(.data$n_na)) %>%
      dplyr::filter(.data$min_na <= max_missing) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"n_na", -"min_na")

  }

  #data %>%
  #  dplyr::add_count(.data$UID, {{ grouping_column }}, wt = is.na(.data$Intensity), name = "n_na") %>%
  #  dplyr::group_by(.data$UID, {{ grouping_column }}) %>%
  #  dplyr::mutate(perc_na = .data$n_na / dplyr::n()) %>%
  #  dplyr::ungroup() %>%
  #  dplyr::group_by(.data$UID) %>%
  #  dplyr::mutate(max_perc_na = max(.data$perc_na)) %>%
  #  dplyr::filter(.data$max_perc_na <= max_missing) %>%
  #  dplyr::ungroup() %>%
  #  dplyr::select(-"n_na", -"perc_na", -"max_perc_na")
}

filter_cv <- function(data, reference_samples, max_cv = 0.2, na_as_zero = TRUE) {
  if (na_as_zero == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ 0, .default = .data$Intensity))
  }

  data %>%
    dplyr::mutate(Intensity_ref = dplyr::case_when(.data$Sample %in% reference_sample ~ .data$Intensity, .default = NA)) %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(cv = stats::sd(.data$Intensity_ref, na.rm = TRUE) / mean(.data$Intensity_ref, na.rm = TRUE)) %>%
    dplyr::filter(.data$cv <= max_cv) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"Intensity_ref", -"cv")
}

filter_blank <- function(data, blank_samples, min_frac = 3) {
  # substitute NA with 0 for better handling:
  # 0/0 = NaN
  # 1/0 = Inf
  # 0/1 = 0

  # primitive test:
  # tibble(frac_sb = c(0, 1, Inf, NaN, 10)) %>% filter(frac_sb >= 3 & !is.nan(frac_sb))

  data <- data %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ 0, .default = .data$Intensity))

  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(
      max_blank = dplyr::case_when(.data$Sample %in% blank_sample ~ .data$Intensity, .default = NA),
      max_blank = max(.data$max_blank, na.rm = TRUE),
      max_sample = dplyr::case_when(!(.data$Sample %in% blank_sample) ~ .data$Intensity, .default = NA),
      max_sample = max(.data$max_sample, na.rm = TRUE),
      frac_sb = .data$max_sample / .data$max_blank
    ) %>%
    dplyr::filter(.data$frac_sb >= min_frac & !is.nan(.data$frac_sb)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"frac_sb", -"max_blank", -"max_sample")
}
