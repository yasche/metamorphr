#' Filter Features based on the absolute number or fraction of samples it was found in
#'
#' @description
#' One of several filter functions. Can be used to filter features based on the number or fraction of samples they are found in.
#' This is usually one of the first steps in metabolomics data analysis and often already performed when the feature table is first created.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#' @param min_found In how many samples must a Feature be found? If `fraction == TRUE`, a value between 0 and 1 (_e.g._, 0.5 if a Feature must be found un at least half the samples). If `fraction == FALSE` the absolute maximum number of samples (_e.g._, 5 if a specific Feature must be found in at least 5 samples).
#' @param fraction Either `TRUE` or `FALSE`. Should `min_found` be the absolute number of samples or a fraction?
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' #Example 1: A feature must be found in at least 50 % of the samples
#' toy_metaboscape %>%
#'   filter_global_mv(min_found = 0.5)
#'
#' #Example 2: A feature must be found in at least 8 samples
#' toy_metaboscape %>%
#'   filter_global_mv(min_found = 8, fraction = FALSE)
filter_global_mv <- function(data, min_found, fraction = TRUE) {
  data <- data %>%
    dplyr::add_count(.data$UID, wt = !is.na(.data$Intensity), name = "not_na") %>%
    dplyr::group_by(.data$UID)

  if (fraction == TRUE) {
    if (min_found > 1) {
      stop("Argument max_missing must be <= 1 if argument fraction is TRUE.")
    }

     data %>%
       dplyr::mutate(perc_not_na = .data$not_na / dplyr::n()) %>%
       dplyr::filter(.data$perc_not_na >= min_found) %>%
       dplyr::ungroup() %>%
       dplyr::select(-"not_na", -"perc_not_na")

  } else {

    data %>%
      dplyr::filter(.data$not_na >= min_found) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"not_na")
  }


}

#' Group-based feature filtering
#'
#' @description
#' One of several filter functions. Similar to `filter_global_mv()` it filters features that are found in a specified number of samples.
#' The key difference is that `filter_grouped_mv()` takes groups into consideration and therefore needs sample metadata.
#' For example, if `fraction = TRUE` and `min_found = 0.75`, a feature must be found in at least 75 % of the samples of at least 1 group.
#' It is very similar to the _Filter features by occurrences in groups_ option in Bruker MetaboScape.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}} with added sample metadata. See ?\code{\link[metamorphr]{create_metadata_skeleton}} for help.
#' @param min_found Defines in how many samples of at least 1 group a Feature must be found not to be filtered out. If `fraction == TRUE`, a value between 0 and 1 (_e.g._, 0.5 if a Feature must be found in at least half the samples of at least 1 group). If `fraction == FALSE` the absolute maximum number of samples (_e.g._, 5 if a specific Feature must be found in at least 5 samples of at least 1 group).
#' @param grouping_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param fraction Either `TRUE` or `FALSE`. Should `min_found` be the absolute number of samples or a fraction?
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' # A Feature must be found in all samples of at least 1 group.
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   filter_grouped_mv(min_found = 1, grouping_column = Group)
filter_grouped_mv <- function(data, min_found, grouping_column, fraction = TRUE) {
  # using injection: https://rlang.r-lib.org/reference/topic-inject.html

  data <- data %>%
    dplyr::add_count(.data$UID, {{ grouping_column }}, wt = !is.na(.data$Intensity), name = "not_na") %>%
    dplyr::ungroup()

  if (fraction == TRUE) {
    if (min_found > 1) {
      stop("Argument max_missing must be <= 1 if argument fraction is TRUE.")
    }

    data %>%
      dplyr::group_by(.data$UID, {{ grouping_column }}) %>%
      dplyr::mutate(perc_not_na = .data$not_na / dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(max_perc_not_na = max(.data$perc_not_na)) %>%
      dplyr::filter(.data$max_perc_not_na >= min_found) %>%
      dplyr::ungroup() %>%
      #print(n = 1000)
      dplyr::select(-"not_na", -"perc_not_na", -"max_perc_not_na")

  } else {

    data %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(max_not_na = max(.data$not_na)) %>%
      dplyr::filter(.data$max_not_na >= min_found) %>%
      dplyr::ungroup() %>%
      #print(n = 1000)
      dplyr::select(-"not_na", -"max_not_na")

  }
}

#' Filter Features based on their coefficient of variation
#'
#' @description
#' One of several filter functions. It filters Features based on their coefficient of variation (CV).
#' It is defined as \eqn{CV = \frac{\sigma }{\mu }} with \eqn{\sigma} = Standard deviation and \eqn{\mu} = Mean.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param max_cv The maximum allowed CV. 0.2 is a reasonable start.
#' @param reference_samples The names of the samples or group which will be used to calculate the CV of a feature. Often Quality Control samples.
#' @param ref_as_group A logical indicating if `reference_samples` are the names of samples or group(s).
#' @param grouping_column Only relevant if `ref_as_group = TRUE`. Which column should be used for grouping reference and non-reference samples? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param na_as_zero Should `NA` be replaced with 0 prior to calculation?
#' Under the hood `filter_cv` calculates the CV by `stats::sd(..., na.rm = TRUE) / mean(..., na.rm = TRUE)`.
#' If there are 3 samples to calculate the CV from and 2 of them are `NA` for a specific feature, then the CV for that Feature will be `NA`
#' if `na_as_zero = FALSE`. This might lead to problems. `na_as_zero = TRUE` is the safer pick.
#' Zeros will be replaced with `NA` after calculation no matter if it is `TRUE` or `FALSE`.
#'
#' @return A filtered tibble.
#' @references <a href="https://en.wikipedia.org/wiki/Coefficient_of_variation">Coefficient of Variation on Wikipedia</a>
#' @export
#'
#' @examples
#' #Example 1: Define reference samples by sample names
#' toy_metaboscape %>%
#'   filter_cv(max_cv = 0.2, reference_samples = c("QC1", "QC2", "QC3"))
#'
#' #Example 2: Define reference samples by group name
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   filter_cv(max_cv = 0.2, reference_samples = "QC", ref_as_group = TRUE, grouping_column = Group)
filter_cv <- function(data, max_cv = 0.2, reference_samples, ref_as_group = FALSE, grouping_column = NULL, na_as_zero = TRUE) {
  #perform  checks
  #if (ref_as_group == TRUE) {
  #  if (is.null(grouping_column)) {
  #    stop("A grouping column must be provided if argument ref_as_group is TRUE. See ?metamorphr::filter_cv for details.")
  #  }
  #}

  if (na_as_zero == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ 0, .default = .data$Intensity))
  }

  if (ref_as_group == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity_ref = dplyr::case_when(dplyr::select(data, {{ grouping_column }}) == reference_samples ~ .data$Intensity, .default = NA))
  } else {
    data <- data %>%
      dplyr::mutate(Intensity_ref = dplyr::case_when(.data$Sample %in% reference_samples ~ .data$Intensity, .default = NA))
  }

  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(cv = stats::sd(.data$Intensity_ref, na.rm = TRUE) / mean(.data$Intensity_ref, na.rm = TRUE)) %>%
    dplyr::filter(.data$cv <= max_cv) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"Intensity_ref", -"cv")
}

#' Filter Features based on their occurrence in blank samples
#'
#' @description
#' One of several filter functions. It filters Features based on their occurrence in blank samples.
#' For example, if `min_frac = 3` the maximum intensity in samples must be at least 3 times as high as in blanks
#' for a Feature not to be filtered out.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param min_frac A numeric defining how many times higher the maximum intensity in samples must be in relation to blanks.
#' @param blank_samples Defines the blanks. If `blank_as_group = FALSE` a character vector containing the names of the blank samples
#' as in the `Sample` column of `data`. If `blank_as_group = TRUE` the name(s) of the group(s) that define blanks, as in the `Group` column of `data`.
#' The latter can only be used if sample metadata is provided.
#' @param blank_as_group A logical indicating if `blank_samples` are the names of samples or group(s).
#' @param grouping_column Only relevant if `blank_as_group = TRUE`. Which column should be used for grouping blank and non-blank samples? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' #Example 1: Define blanks by sample name
#' toy_metaboscape %>%
#'   filter_blank(blank_samples = c("Blank1", "Blank2"), blank_as_group = FALSE, min_frac = 3)
#'
#' #Example 2: Define blanks by group name
#' #toy_metaboscape %>%
#' #  join_metadata(toy_metaboscape_metadata) %>%
#' #  filter_blank(blank_samples = "blank",
#' #               blank_as_group = TRUE,
#' #               min_frac = 3,
#' #               grouping_column = Group)
filter_blank <- function(data, min_frac = 3, blank_samples, blank_as_group = FALSE, grouping_column = NULL) {
  # substitute NA with 0 for better handling:
  # 0/0 = NaN
  # 1/0 = Inf
  # 0/1 = 0

  data <- data %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ 0, .default = .data$Intensity))

  if (blank_as_group == FALSE) {
    data <- data %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(
        max_blank = dplyr::case_when(.data$Sample %in% blank_samples ~ .data$Intensity, .default = NA),
        max_blank = max(.data$max_blank, na.rm = TRUE),
        max_sample = dplyr::case_when(!(.data$Sample %in% blank_samples) ~ .data$Intensity, .default = NA),
        max_sample = max(.data$max_sample, na.rm = TRUE),
        frac_sb = .data$max_sample / .data$max_blank
      )
  } else {
    grouping_column_str <- rlang::expr_label(substitute(grouping_column))
    grouping_column_str <- gsub("`", "", grouping_column_str)

    if (is.null(grouping_column_str)) {
      stop("grouping_column can't be NULL if `blank_as_group = TRUE`.\nUsually `group_column = Group`.")
    }

    if (!(grouping_column_str %in% colnames(data))) {
      stop(paste(grouping_column_str, " column was not found in 'data'.\nDid you forget to add metadata or did you provide `group_column` with quotation marks?"))
    }

    data <- data %>%
      dplyr::mutate(
        max_blank = dplyr::case_when(dplyr::select(data, {{ grouping_column }}) == blank_samples ~ .data$Intensity, .default = NA),
        max_sample = dplyr::case_when(!(dplyr::select(data, {{ grouping_column }}) == blank_samples) ~ .data$Intensity, .default = NA)
      ) %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(
        max_blank = max(.data$max_blank, na.rm = TRUE),
        max_sample = max(.data$max_sample, na.rm = TRUE),
        frac_sb = .data$max_sample / .data$max_blank
      )
  }
  data %>%
    #how should the case 0/0 be handled? -> 0/0 = NaN -> currently filtered out
    #other approach: replace 0/0 with 0
    dplyr::filter(.data$frac_sb >= min_frac & !is.nan(.data$frac_sb)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"frac_sb", -"max_blank", -"max_sample")
}
