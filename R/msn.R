#' Scale intensities in MSn spectra to the highest value within each spectrum
#'
#' @description
#' Scale the intensity of each peak in an MSn spectrum to that of the highest peak. MSn spectra are required to use this function.
#' See \code{\link[metamorphr]{read_mgf}}.
#'
#' \strong{Important Note}
#'
#' Please note that existing MSn spectra in `data` will be overwritten.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param scale_to A `numeric` that specifies to which number the highest signal in each spectrum will be scaled.
#'
#' @returns A tibble with scaled MSn spectra.
#' @export
#'
#' @examples
#' toy_mgf %>%
#'   msn_scale()
msn_scale <- function(data, scale_to = 100) {
  col_order <- names(data)

  rownums <- 1:nrow(data)

  data %>%
    dplyr::mutate(row_number = .env$rownums) %>%
    dplyr::group_by(.data$MSn) %>%
    tidyr::nest() %>%
    dplyr::mutate(MSn = purrr::map(.data$MSn, internal_msn_scale, scale_to = .env$scale_to)) %>%
    tidyr::unnest("data") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$row_number) %>%
    dplyr::select(-"row_number") %>%
    dplyr::relocate(dplyr::all_of(col_order))
}



#' Calculate neutral losses from precursor ion mass and fragment ion masses
#'
#' @description
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
#'   msn_calc_nl(m_z_col = PEPMASS)
msn_calc_nl <- function(data, m_z_col) {
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


#' Calculate neutral losses from precursor ion mass and fragment ion masses
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#'
#' `calc_neutral_loss()` is fully replaced by \code{\link[metamorphr]{msn_calc_nl}}.
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
  lifecycle::deprecate_warn("0.3.0", "calc_neutral_loss()", "msn_calc_nl()")
  msn_calc_nl(data, {{ m_z_col }})
}
