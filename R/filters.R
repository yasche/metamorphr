filter_global_mv <- function(data, max_na) {
  data %>%
    dplyr::add_count(.data$UID, wt = is.na(.data$Intensity), name = "n_na") %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(perc_na = .data$n_na / dplyr::n()) %>%
    dplyr::filter(.data$perc_na <= max_na) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"n_na", -"perc_na")
}

filter_grouped_mv <- function(data, grouping_column, max_na) {
  # using injection: https://rlang.r-lib.org/reference/topic-inject.html

  data %>%
    dplyr::add_count(.data$UID, {{ grouping_column }}, wt = is.na(.data$Intensity), name = "n_na") %>%
    dplyr::group_by(.data$UID, {{ grouping_column }}) %>%
    dplyr::mutate(perc_na = .data$n_na / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(max_perc_na = max(.data$perc_na)) %>%
    dplyr::filter(.data$max_perc_na <= max_na) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"n_na", -"perc_na", -"max_perc_na")
}

filter_cv <- function(data, reference_sample, max_cv = 0.2, na_as_zero = TRUE) {
  if (na_as_zero == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ 0, .default = .data$Intensity))
  }

  data %>%
    dplyr::mutate(Intensity_ref = dplyr::case_when(.data$Sample == reference_sample ~ .data$Intensity, .default = NA)) %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(cv = stats::sd(.data$Intensity_ref, na.rm = TRUE) / mean(.data$Intensity_ref, na.rm = TRUE)) %>%
    dplyr::filter(.data$cv <= max_cv) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"Intensity_ref", -"cv")
}

filter_blank <- function(data, blank_sample, min_frac = 3) {
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
      max_blank = dplyr::case_when(.data$Sample == blank_sample ~ .data$Intensity, .default = NA),
      max_blank = max(.data$max_blank, na.rm = TRUE),
      max_sample = dplyr::case_when(.data$Sample != blank_sample ~ .data$Intensity, .default = NA),
      max_sample = max(.data$max_sample, na.rm = TRUE),
      frac_sb = .data$max_sample / .data$max_blank
    ) %>%
    dplyr::filter(.data$frac_sb >= min_frac & !is.nan(.data$frac_sb)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"frac_sb", -"max_blank", -"max_sample")
}
