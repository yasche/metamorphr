#' Join a featuretable and sample metadata
#'
#' @description
#' Joins a featuretable and associated sample metadata. Basically a wrapper around \code{\link[dplyr]{left_join}} where `by = "Sample"`.
#'
#' @param data A feature table created with \code{\link[metamorphr]{read_featuretable}}
#' @param metadata Sample metadata created with \code{\link[metamorphr]{create_metadata_skeleton}}
#'
#' @return A tibble with added sample metadata.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata)
join_metadata <- function(data, metadata) {
  dplyr::left_join(data, metadata, by = "Sample")
}



#' General information about a feature table and sample-wise summary
#'
#' @description
#' Information about a feature table. Prints information to the console (number of samples, number of features and if applicable number of groups,
#' replicates and batches) and returns a sample-wise summary as a list.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_samples_max How many Samples should be printed to the console?
#' @param n_features_max How many Features should be printed to the console?
#' @param n_groups_max How many groups should be printed to the console?
#' @param n_batches_max How many Batches should be printed to the console?
#'
#' @return A sample-wise summary as a list.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   summary_featuretable()
summary_featuretable <- function(data, n_samples_max = 5, n_features_max = 5, n_groups_max = 5, n_batches_max = 5) {
  column_names <- colnames(data)

  samples <- summary_featuretable_pull(data = data, select_what = "Sample")
  features <- summary_featuretable_pull(data = data, select_what = 2)

  n_samples <- length(samples)
  n_features <- length(features)

  summary_featuretable_cat(txt = samples, title = "Samples", n = n_samples, n_max = n_samples_max)
  summary_featuretable_cat(txt = features, title = "Features", n = n_features, n_max = n_samples_max)

  # is metadata present?
  if ("Group" %in% column_names) {
    groups <- summary_featuretable_pull(data = data, select_what = "Group")

    n_groups <- groups %>%
      length()

    summary_featuretable_cat(txt = groups, title = "Groups", n = n_groups, n_max = n_groups_max)
  }

  if ("Replicate" %in% column_names) {
    replicates <- summary_featuretable_pull(data = data, select_what = "Replicate")

    n_replicates <- length(replicates)

    if (n_replicates > 1) {
      rlang::inform(message = paste0(crayon::blue("Replicates detected: ", min(replicates), "...", max(replicates), "\n", sep = "")))
    }
  }

  if ("Batch" %in% column_names) {
    batches <- summary_featuretable_pull(data = data, select_what = "Batch")

    n_batches <- length(batches)

    summary_featuretable_cat(txt = batches, title = "Batches", n = n_batches, n_max = n_batches_max)
  }

  n_nas <- data %>%
    dplyr::filter(is.na(.data$Intensity)) %>%
    nrow()

  n_total <- nrow(data)

  rlang::inform(message = paste0(crayon::green(paste0(as.character(round((n_nas / n_total) * 100)), " % missing values (NA): ", sep = "")), paste0(as.character(n_nas), " out of ", as.character(n_total), "."), "\n", sep = ""))

  data <- data %>%
    dplyr::group_by(.data$Sample) %>%
    tidyr::nest() %>%
    dplyr::mutate(summary = purrr::map(.data$data, function(x) {
      summary(x$Intensity)
    })) %>%
    dplyr::pull(summary)

  names(data) <- samples

  data
}


summary_featuretable_pull <- function(data, select_what) {
  data %>%
    dplyr::select(dplyr::all_of(select_what)) %>%
    dplyr::distinct() %>%
    dplyr::pull()
}


summary_featuretable_cat <- function(txt, title, n, n_max) {
  if (n > n_max) {
    txt <- c(utils::head(txt, n = n_max))
  }

  rlang::inform(message = paste0(crayon::blue(as.character(n), " ", title, ": ", sep = ""), paste(txt, collapse = ", "), "\n", sep = ""))
  if (n > n_max) {
    rlang::inform(message = paste0(crayon::silver("# ", as.character(n - n_max), " more ", tolower(title), "\n", sep = "")))
    rlang::inform(message = paste0(crayon::silver("# ", "Use the n_", tolower(title), "_max", " argument to see more", "\n", sep = "")))
  }
}


#' Calculate neutral losses from precursor ion mass and fragment ion masses
#'
#' Calculate neutral loss spectra for all ions with available MSn spectra in `data`. To calculate neutral losses, MSn spectra are required.
#' See \code{\link[metamorphr]{read_mgf}}. This step is required for subsequent filtering based on
#' neutral losses (\code{\link[metamorphr]{filter_neutral_loss}}). Resulting neutral loss spectra are stored in tibbles in a new list column named `Neutral_Loss`.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param m_z_col Which column holds the precursor m/z? Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @returns A tibble with added neutral loss spectra. A new list column is created named `Neutral_Loss`.
#' @export
#'
#' @examples
#' toy_mgf %>%
#'   calc_neutral_loss(m_z_col = PEPMASS)
calc_neutral_loss <- function(data, m_z_col) {
  col_order <- names(data)

  rownums <- 1:nrow(data)

  data %>%
    dplyr::mutate(row_number = .env$rownums) %>%
    dplyr::group_by(.data$MSn, {{ m_z_col }}) %>%
    tidyr::nest() %>%
    dplyr::mutate(Neutral_Loss = purrr::map2({{ m_z_col }}, .data$MSn, internal_calc_neutral_loss, .progress = TRUE)) %>%
    tidyr::unnest("data") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$row_number) %>%
    dplyr::select(-"row_number") %>%
    dplyr::relocate(dplyr::all_of(col_order))
}


#' Calculate the monoisotopic mass from a given formula
#'
#' @description
#' Calculates the monoisotopic mass from a given formula. If only the element symbols are provided, the calculated mass corresponds to that of a molecule made up from the most abundant isotopes.
#' Other isotopes can also be provided (e.g., <sup>13</sup>C, instead of the naturally most abundant <sup>12</sup>C). See the samples for details.
#'
#'
#' @param formula A formula as a string.
#'
#' @returns The monoisotopic mass of the formula.
#' @export
#'
#' @examples
#' # The monoisotopic mass is calculated with the most abundant isotopes
#' # if only the element symbols are provided:
#' formula_to_mass("CH4")
#' formula_to_mass("NH3")
#' formula_to_mass("C10H17N3O6S")
#'
#' # Other isotopes can be provided as follows:
#' formula_to_mass("[13C]H4")
#' formula_to_mass("[15N]H3")
#'
#' # Every isotope, including the most abundant ones, can be named explicitly.
#' # Compare:
#' formula_to_mass("[14N][1H]3")
#' formula_to_mass("NH3")
#'
#' # The function also supports brackets and nested brackets:
#' formula_to_mass("(CH3)2")
#' formula_to_mass("(((CH3)2N)3C)2")
#' formula_to_mass("((([13C]H3)2N)3C)2")
formula_to_mass <- function(formula) {
  internal_formula_to_mass(formula,
                           special_isos_lookup = exact_special_isos_lookup,
                           other_atoms_multi_lookup = exact_other_atoms_multi_lookup,
                           other_atoms_single_lookup = exact_other_atoms_single_lookup)
}


#' Calculate the Kendrick mass
#'
#' @description
#' Calculate the Kendrick mass for a given mass (or m/z) and repeating unit.
#' The Kendrick mass is a rescaled mass, that usually sets CH2 = 14 but other
#' repeating units can also be used. It is usefull for the visual identification
#' of potential homologues. See the References section for more information.
#' The Kendrick mass is not to be confused with the Kendrick mass defect
#' (KMD, \code{\link[metamorphr]{calc_kmd}}) and
#' the nominal Kendrick mass (\code{\link[metamorphr]{calc_nominal_km}}).
#'
#'
#' @param mass A molecular mass (or m/z).
#' @param repeating_unit The formula of the repeating unit, given as a string.
#'
#' @returns The Kendrick mass.
#' @export
#'
#' @references \itemize{
#'  \item \href{https://en.wikipedia.org/wiki/Kendrick_mass}{Kendrick mass on Wikipedia}
#'  \item Edward Kendrick, \emph{Anal. Chem.} \strong{1963}, \emph{35}, 2146–2154.
#'  \item C. A. Hughey, C. L. Hendrickson, R. P. Rodgers, A. G. Marshall, K. Qian, \emph{Anal. Chem.} \strong{2001}, \emph{73}, 4676–4681.
#' }
#'
#' @examples
#' # Calculate the Kendrick masses for two measured masses with
#' # CH2 as the repeating unit.
#' # See Hughey et al. in the References section above
#'
#' calc_km(c(351.3269, 365.3425))
#'
#' # Construct a KMD plot from m/z values.
#' # RT is mapped to color and the feature-wise maximum intensity to size.
#' # Note that in the publication by Hughey et al., the nominal Kendrick mass
#' # is used on the x-axis instead of the exact Kendrick mass.
#' # See ?calc_nominal_km.
#'
#' toy_metaboscape %>%
#'   dplyr::group_by(UID, `m/z`, RT) %>%
#'   dplyr::summarise(max_int = max(Intensity, na.rm = TRUE)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(KMD = calc_kmd(`m/z`),
#'                 KM = calc_km(`m/z`)) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = KM,
#'                                y = KMD,
#'                                size = max_int,
#'                                color = RT)) +
#'     ggplot2::geom_point()
calc_km <- function(mass, repeating_unit = "CH2") {
  mass * internal_formula_to_mass(repeating_unit,
                                  iso_special_isos_lookup,
                                  iso_other_atoms_multi_lookup,
                                  iso_other_atoms_single_lookup) /
    internal_formula_to_mass(repeating_unit,
                             exact_special_isos_lookup,
                             exact_other_atoms_multi_lookup,
                             exact_other_atoms_single_lookup)
}

#' Calculate the nominal Kendrick mass
#'
#' @description
#' The nominal Kendrick mass is the Kendrick mass
#' (\code{\link[metamorphr]{calc_km}}), rounded up to the nearest
#' whole number. The nominal Kendrick mass and the Kendrick mass are both required
#' to calculate the Kendrick mass defect (KMD).
#' The nominal Kendrick mass is not to be confused with the Kendrick mass defect
#' (\code{\link[metamorphr]{calc_kmd}}) and
#' the Kendrick mass (\code{\link[metamorphr]{calc_km}}).
#'
#' @param mass A molecular mass (or m/z).
#' @param repeating_unit The formula of the repeating unit, given as a string.
#'
#' @returns The nominal Kendrick mass.
#' @export
#'
#' @references \itemize{
#'  \item \href{https://en.wikipedia.org/wiki/Kendrick_mass}{Kendrick mass on Wikipedia}
#'  \item Edward Kendrick, \emph{Anal. Chem.} \strong{1963}, \emph{35}, 2146–2154.
#'  \item C. A. Hughey, C. L. Hendrickson, R. P. Rodgers, A. G. Marshall, K. Qian, \emph{Anal. Chem.} \strong{2001}, \emph{73}, 4676–4681.
#' }
#'
#' @examples
#' # Calculate the nominal Kendrick masses for two measured masses with
#' # CH2 as the repeating unit.
#' # See Hughey et al. in the References section above
#'
#' calc_nominal_km(c(351.3269, 365.3425))
#'
#' # Construct a KMD plot from m/z values.
#' # RT is mapped to color and the feature-wise maximum intensity to size.
#'
#' toy_metaboscape %>%
#'   dplyr::group_by(UID, `m/z`, RT) %>%
#'   dplyr::summarise(max_int = max(Intensity, na.rm = TRUE)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(KMD = calc_kmd(`m/z`),
#'                 `nominal KM` = calc_nominal_km(`m/z`)) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = `nominal KM`,
#'                                y = KMD,
#'                                size = max_int,
#'                                color = RT)) +
#'     ggplot2::geom_point()
calc_nominal_km <- function(mass, repeating_unit = "CH2") {
  ceiling(calc_km(mass, repeating_unit = repeating_unit))
}

#' Calculate the Kendrick mass defect (KMD)
#'
#' @description
#' The Kendrick mass defect (KMD) is calculated by subtracting the Kendrick mass
#' (\code{\link[metamorphr]{calc_km}}) from the nominal Kendrick mass
#' (\code{\link[metamorphr]{calc_nominal_km}}). The the References section for
#' more information.
#'
#'
#' @param mass A molecular mass (or m/z).
#' @param repeating_unit The formula of the repeating unit, given as a string.
#'
#' @returns The Kendrick mass defect (KMD)
#' @export
#'
#' @references \itemize{
#'  \item \href{https://en.wikipedia.org/wiki/Kendrick_mass}{Kendrick mass on Wikipedia}
#'  \item Edward Kendrick, \emph{Anal. Chem.} \strong{1963}, \emph{35}, 2146–2154.
#'  \item C. A. Hughey, C. L. Hendrickson, R. P. Rodgers, A. G. Marshall, K. Qian, \emph{Anal. Chem.} \strong{2001}, \emph{73}, 4676–4681.
#' }
#'
#' @examples
#' # Calculate the Kendrick mass defects for two measured masses with
#' # CH2 as the repeating unit.
#' # See Hughey et al. in the References section above
#'
#' calc_kmd(c(351.3269, 365.3425))
#'
#' # Construct a KMD plot from m/z values.
#' # RT is mapped to color and the feature-wise maximum intensity to size.
#'
#' toy_metaboscape %>%
#'   dplyr::group_by(UID, `m/z`, RT) %>%
#'   dplyr::summarise(max_int = max(Intensity, na.rm = TRUE)) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(KMD = calc_kmd(`m/z`),
#'                 `nominal KM` = calc_nominal_km(`m/z`)) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = `nominal KM`,
#'                                y = KMD,
#'                                size = max_int,
#'                                color = RT)) +
#'     ggplot2::geom_point()
calc_kmd <- function(mass, repeating_unit = "CH2") {
  calc_nominal_km(mass, repeating_unit = repeating_unit) - calc_km(mass, repeating_unit = repeating_unit)
}
