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
