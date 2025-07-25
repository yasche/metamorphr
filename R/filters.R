#' Filter Features based on the absolute number or fraction of samples it was found in
#'
#' @description
#' Filters features based on the number or fraction of samples they are found in.
#' This is usually one of the first steps in metabolomics data analysis and often already performed when the feature table is first created from the raw spectral files..
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#' @param min_found In how many samples must a Feature be found? If `fraction == TRUE`, a value between 0 and 1 (_e.g._, 0.5 if a Feature must be found in at least half the samples). If `fraction == FALSE` the absolute maximum number of samples (_e.g._, 5 if a specific Feature must be found in at least 5 samples).
#' @param fraction Either `TRUE` or `FALSE`. Should `min_found` be the absolute number of samples or a fraction?
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' # Example 1: A feature must be found in at least 50 % of the samples
#' toy_metaboscape %>%
#'   filter_global_mv(min_found = 0.5)
#'
#' # Example 2: A feature must be found in at least 8 samples
#' toy_metaboscape %>%
#'   filter_global_mv(min_found = 8, fraction = FALSE)
filter_global_mv <- function(data, min_found = 0.5, fraction = TRUE) {
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
#' Similar to \code{\link[metamorphr]{filter_global_mv}} it filters features that are found in a specified number of samples.
#' The key difference is that `filter_grouped_mv()` takes groups into consideration and therefore needs sample metadata.
#' For example, if `fraction = TRUE` and `min_found = 0.5`, a feature must be found in at least 50 % of the samples of at least 1 group.
#' It is very similar to the _Filter features by occurrences in groups_ option in Bruker MetaboScape.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}} with added sample metadata. See ?\code{\link[metamorphr]{create_metadata_skeleton}} for help.
#' @param min_found Defines in how many samples of at least 1 group a Feature must be found not to be filtered out. If `fraction == TRUE`, a value between 0 and 1 (_e.g._, 0.5 if a Feature must be found in at least half the samples of at least 1 group). If `fraction == FALSE` the absolute maximum number of samples (_e.g._, 5 if a specific Feature must be found in at least 5 samples of at least 1 group).
#' @param group_column Which column should be used for grouping? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param fraction Either `TRUE` or `FALSE`. Should `min_found` be the absolute number of samples or a fraction?
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' # A Feature must be found in all samples of at least 1 group.
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   filter_grouped_mv(min_found = 1, group_column = Group)
filter_grouped_mv <- function(data, min_found = 0.5, group_column = .data$Group, fraction = TRUE) {
  # using injection: https://rlang.r-lib.org/reference/topic-inject.html

  data <- data %>%
    dplyr::add_count(.data$UID, {{ group_column }}, wt = !is.na(.data$Intensity), name = "not_na") %>%
    dplyr::ungroup()

  if (fraction == TRUE) {
    if (min_found > 1) {
      stop("Argument max_missing must be <= 1 if argument fraction is TRUE.")
    }

    data %>%
      dplyr::group_by(.data$UID, {{ group_column }}) %>%
      dplyr::mutate(perc_not_na = .data$not_na / dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(max_perc_not_na = max(.data$perc_not_na)) %>%
      dplyr::filter(.data$max_perc_not_na >= min_found) %>%
      dplyr::ungroup() %>%
      # print(n = 1000)
      dplyr::select(-"not_na", -"perc_not_na", -"max_perc_not_na")
  } else {
    data %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(max_not_na = max(.data$not_na)) %>%
      dplyr::filter(.data$max_not_na >= min_found) %>%
      dplyr::ungroup() %>%
      # print(n = 1000)
      dplyr::select(-"not_na", -"max_not_na")
  }
}

#' Filter Features based on their coefficient of variation
#'
#' @description
#' Filters Features based on their coefficient of variation (CV).
#' The CV is defined as \eqn{CV = \frac{s_i}{\overline{x_i}}} with \eqn{s_i} = Standard deviation of sample \eqn{i} and \eqn{\overline{x_i}} = Mean of sample \eqn{i}.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param reference_samples The names of the samples or group which will be used to calculate the CV of a feature. Usually Quality Control samples.
#' @param max_cv The maximum allowed CV. 0.2 is a reasonable start.
#' @param ref_as_group A logical indicating if `reference_samples` are the names of samples or group(s).
#' @param group_column Only relevant if `ref_as_group = TRUE`. Which column should be used for grouping reference and non-reference samples? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param na_as_zero Should `NA` be replaced with 0 prior to calculation?
#' Under the hood `filter_cv` calculates the CV by `stats::sd(..., na.rm = TRUE) / mean(..., na.rm = TRUE)`.
#' If there are 3 samples to calculate the CV from and 2 of them are `NA` for a specific feature, then the CV for that Feature will be `NA`
#' if `na_as_zero = FALSE`. This might lead to problems. `na_as_zero = TRUE` is the safer pick.
#' Zeros will be replaced with `NA` after calculation no matter if it is `TRUE` or `FALSE`.
#'
#' @return A filtered tibble.
#' @references \href{https://en.wikipedia.org/wiki/Coefficient_of_variation}{Coefficient of Variation on Wikipedia}
#' @export
#'
#' @examples
#' # Example 1: Define reference samples by sample names
#' toy_metaboscape %>%
#'   filter_cv(max_cv = 0.2, reference_samples = c("QC1", "QC2", "QC3"))
#'
#' # Example 2: Define reference samples by group name
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   filter_cv(max_cv = 0.2, reference_samples = "QC", ref_as_group = TRUE, group_column = Group)
filter_cv <- function(data, reference_samples, max_cv = 0.2, ref_as_group = FALSE, group_column = NULL, na_as_zero = TRUE) {
  # perform  checks
  # if (ref_as_group == TRUE) {
  #  if (is.null(group_column)) {
  #    stop("A grouping column must be provided if argument ref_as_group is TRUE. See ?metamorphr::filter_cv for details.")
  #  }
  # }

  if (na_as_zero == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ 0, .default = .data$Intensity))
  }

  if (ref_as_group == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity_ref = dplyr::case_when({{ group_column }} == reference_samples ~ .data$Intensity, .default = NA))
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
#' Filters Features based on their occurrence in blank samples.
#' For example, if `min_frac = 3` the maximum intensity in samples must be at least 3 times as high as in blanks
#' for a Feature not to be filtered out.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param blank_samples Defines the blanks. If `blank_as_group = FALSE` a character vector containing the names of the blank samples
#' as in the `Sample` column of `data`. If `blank_as_group = TRUE` the name(s) of the group(s) that define blanks, as in the `Group` column of `data`.
#' The latter can only be used if sample metadata is provided.
#' @param min_frac A numeric defining how many times higher the maximum intensity in samples must be in relation to blanks.
#' @param blank_as_group A logical indicating if `blank_samples` are the names of samples or group(s).
#' @param group_column Only relevant if `blank_as_group = TRUE`. Which column should be used for grouping blank and non-blank samples? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' # Example 1: Define blanks by sample name
#' toy_metaboscape %>%
#'   filter_blank(blank_samples = c("Blank1", "Blank2"), blank_as_group = FALSE, min_frac = 3)
#'
#' # Example 2: Define blanks by group name
#' # toy_metaboscape %>%
#' #  join_metadata(toy_metaboscape_metadata) %>%
#' #  filter_blank(blank_samples = "blank",
#' #               blank_as_group = TRUE,
#' #               min_frac = 3,
#' #               group_column = Group)
filter_blank <- function(data, blank_samples, min_frac = 3, blank_as_group = FALSE, group_column = NULL) {
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
    group_column_str <- rlang::as_label(rlang::enquo(group_column))
    #group_column_str <- gsub("`", "", group_column_str)

    if (group_column_str == "NULL") {
      stop("group_column can't be NULL if `blank_as_group = TRUE`.\nUsually `group_column = Group`.")
    }

    if (!(group_column_str %in% colnames(data))) {
      stop(paste(group_column_str, " column was not found in 'data'.\nDid you forget to add metadata or did you provide `group_column` with quotation marks?"))
    }

    data <- data %>%
      dplyr::mutate(
        max_blank = dplyr::case_when({{ group_column }} %in% blank_samples ~ .data$Intensity, .default = NA),
        max_sample = dplyr::case_when(!({{ group_column }} %in% blank_samples) ~ .data$Intensity, .default = NA)
      ) %>%
      dplyr::group_by(.data$UID) %>%
      dplyr::mutate(
        max_blank = max(.data$max_blank, na.rm = TRUE),
        max_sample = max(.data$max_sample, na.rm = TRUE),
        frac_sb = .data$max_sample / .data$max_blank
      )
  }
  data %>%
    # how should the case 0/0 be handled? -> 0/0 = NaN -> currently filtered out
    # other approach: replace 0/0 with 0
    dplyr::filter(.data$frac_sb >= min_frac & !is.nan(.data$frac_sb)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0)) %>%
    dplyr::select(-"frac_sb", -"max_blank", -"max_sample")
}


#' Filter Features based on occurrence of fragment ions
#'
#' @description
#' Filters Features based on the presence of MSn fragments. This can help, for example with the identification of potential homologous molecules.
#'
#' @param data A data frame containing MSn spectra.
#' @param fragments A numeric. Exact mass of the fragment(s) to filter by.
#' @param min_found How many of the `fragments` must be found in order to keep the row? If `min_found = length(fragments)`, all fragments must be found.
#' @param tolerance A numeric. The tolerance to apply to the fragments. Either an absolute value in Da (if `tolerance_type = "absolute"`) or in ppm (if `tolerance_type = "ppm"`).
#' @param tolerance_type Either `"absolute"` or `"ppm"`. Should the tolerance be an absolute value or in ppm?
#' @param show_progress A `logical` indicating whether the progress of the filtering should be printed to the console. Only important for large tibbles.
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' # all of the given fragments (3) must be found
#' # returns the first row of toy_mgf
#' toy_mgf %>%
#'   filter_msn(fragments = c(12.3456, 23.4567, 34.5678), min_found = 3)
#'
#' # all of the given fragments (3) must be found
#' # returns an empty tibble because the third fragment
#' # of row 1 (34.5678)
#' # is outside of the tolerance (5 ppm):
#' # Lower bound:
#' # 34.5688 - 34.5688 * 5 / 1000000 = 34.5686
#' # Upper bound:
#' # 34.5688 + 34.5688 * 5 / 1000000 = 34.5690
#' toy_mgf %>%
#'   filter_msn(fragments = c(12.3456, 23.4567, 34.5688), min_found = 3)
#'
#' # only 2 of the 3 fragments must be found
#' # returns the first row of toy_mgf
#' toy_mgf %>%
#'   filter_msn(fragments = c(12.3456, 23.4567, 34.5688), min_found = 2)
filter_msn <- function(data, fragments, min_found, tolerance = 5, tolerance_type = "ppm", show_progress = TRUE) {
  internal_filter_msn_nl(data = data,
                         fragments = fragments,
                         min_found = min_found,
                         tolerance = tolerance,
                         tolerance_type = tolerance_type,
                         show_progress = show_progress,
                         msn_col = .data$MSn)
}

#' Filter Features based on occurrence of neutral losses
#'
#' The occurrence of characteristic neutral losses can help with the putative annotation of molecules. See the Reference section for an example.
#'
#' @param data A data frame containing MSn spectra.
#' @param losses A numeric. Exact mass of the fragment(s) to filter by.
#' @param min_found How many of the `fragments` must be found in order to keep the row? If `min_found = length(fragments)`, all fragments must be found.
#' @param tolerance A numeric. The tolerance to apply to the fragments. Either an absolute value in Da (if `tolerance_type = "absolute"`) or in ppm (if `tolerance_type = "ppm"`).
#' @param tolerance_type Either `"absolute"` or `"ppm"`. Should the tolerance be an absolute value or in ppm?
#' @param show_progress A `logical` indicating whether the progress of the filtering should be printed to the console. Only important for large tibbles.
#'
#' @returns A filtered tibble.
#' @export
#'
#' @references \itemize{
#'  \item A. Brink, F. Fontaine, M. Marschmann, B. Steinhuber, E. N. Cece, I. Zamora, A. Pähler, \emph{Rapid Commun. Mass Spectrom.} \strong{2014}, \emph{28}, 2695–2703, DOI \href{https://doi.org/10.1002/rcm.7062}{10.1002/rcm.7062}.
#' }
#'
#' @examples
#' # neutral losses must be calculated first
#' toy_mgf_nl <- toy_mgf %>%
#'   calc_neutral_loss(m_z_col = PEPMASS)
#'
#' # all of the given losses (3) must be found
#' # returns the first row of toy_mgf
#' toy_mgf_nl %>%
#'   filter_neutral_loss(losses = c(11.1111, 22.2222, 33.3333), min_found = 3)
#'
#' # all of the given fragments (3) must be found
#' # returns an empty tibble because the third loss
#' # of row 1 (33.3333)
#' # is outside of the tolerance (10 ppm):
#' # Lower bound:
#' # 33.4333 - 33.4333 * 5 / 1000000 = 33.4333
#' # Upper bound:
#' # 33.4333 + 33.4333 * 5 / 1000000 = 33.4336
#' toy_mgf_nl %>%
#'   filter_neutral_loss(losses = c(11.1111, 22.2222, 33.4333), min_found = 3)
#'
#' # only 2 of the 3 fragments must be found
#' # returns the first row of toy_mgf
#' toy_mgf_nl %>%
#'   filter_neutral_loss(losses = c(11.1111, 22.2222, 33.4333), min_found = 2)
filter_neutral_loss <- function(data, losses, min_found, tolerance = 10, tolerance_type = "ppm", show_progress = TRUE) {
  fragments <- losses

  internal_filter_msn_nl(data = data,
                         fragments = fragments,
                         min_found = min_found,
                         tolerance = tolerance,
                         tolerance_type = tolerance_type,
                         show_progress = show_progress,
                         msn_col = .data$Neutral_Loss)
}

#' Filter Features based on their mass-to-charge ratios
#'
#' @description
#' Facilitates filtering by given mass-to-charge ratios (m/z) with a defined tolerance. Can also be used to filter based on exact mass.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param m_z_col Which column holds the precursor m/z (or exact mass)? Uses \code{\link[rlang]{args_data_masking}}.
#' @param masses The mass(es) to filter by.
#' @param tolerance A numeric. The tolerance to apply to the masses Either an absolute value in Da (if `tolerance_type = "absolute"`) or in ppm (if `tolerance_type = "ppm"`).
#' @param tolerance_type Either `"absolute"` or `"ppm"`. Should the tolerance be an absolute value or in ppm?
#'
#' @returns A filtered tibble.
#' @export
#'
#' @examples
#' # Use a tolerance of plus or minus 5 ppm
#' toy_metaboscape %>%
#'   filter_mz(m_z_col = `m/z`, 162.1132, tolerance = 5, tolerance_type = "ppm")
#'
#' # Use a tolerance of plus or minus 0.005 Da
#' toy_metaboscape %>%
#'   filter_mz(m_z_col = `m/z`, 162.1132, tolerance = 0.005, tolerance_type = "absolute")
filter_mz <- function(data, m_z_col, masses, tolerance = 5, tolerance_type = "ppm") {
  dplyr::filter(data, !!internal_create_mz_filter_call(m_z_col = {{ m_z_col }}, masses = masses, tolerance = tolerance, tolerance_type = tolerance_type))
}
