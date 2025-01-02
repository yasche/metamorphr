#' Impute missing values by replacing them with the lowest observed intensity (global)
#'
#' @description
#' Replace missing intensity values (`NA`) with the lowest observed intensity.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
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
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
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
#' Replace missing intensity values (`NA`) with the Feature mean of non-`NA` values. For example, if a Feature has the measured intensities `NA, 1, NA, 3, 2` in samples 1-5,
#' the intensities after `impute_mean()` would be `2, 1, 2, 3, 2`.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
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
#' Replace missing intensity values (`NA`) with the Feature median of non-`NA` values. For example, if a Feature has the measured intensities `NA, 1, NA, 3, 2` in samples 1-5,
#' the intensities after `impute_median()` would be `2, 1, 2, 3, 2`.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
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
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
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
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
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

#' Impute missing values using nearest neighbor averaging
#'
#' @description
#' Basically a wrapper function around `impute::impute.knn()`. Imputes missing values using the k-th nearest neighbor algorithm.
#'
#'
#' Note that the function ln-transforms the data prior to imputation and transforms it back to the original scale afterwards. **Please do not do it manually prior to calling `impute_knn()`!**
#' See References for more information.
#'
#' <b>Important Note</b><br>
#' `impute_knn()` depends on the `impute` package from Bioconductor. If `metamorphr` was installed via `install.packages()`, dependencies from Bioconductor were not
#' automatically installed. When `impute_knn()` is called without the `impute` package installed, you should be asked if you want to install `pak` and `impute`.
#' If you want to use `impute_knn()` you have to install those. In case you run into trouble with the automatic installation, please install `impute` manually. See
#' <a href = "https://bioconductor.org/packages/release/bioc/html/impute.html">impute: Imputation for microarray data</a> for instructions on manual installation.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param ... Additional parameters passed to A tidy tibble created by \code{\link[impute]{impute.knn}}.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references
#' <ul>
#' <li>Robert Tibshirani Trevor Hastie, <b>2017</b>, DOI <a href = "https://doi.org/10.18129/B9.BIOC.IMPUTE">10.18129/B9.BIOC.IMPUTE</a>.</li>
#' <li>J. Khan, J. S. Wei, M. Ringnér, L. H. Saal, M. Ladanyi, F. Westermann, F. Berthold, M. Schwab, C. R. Antonescu, C. Peterson, P. S. Meltzer, <i>Nat Med</i> <b>2001</b>, <i>7</i>, 673–679, DOI <a href = "https://doi.org/10.1038/89044">10.1038/89044</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_knn()
impute_knn <- function(data, ...) {
  #impute is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  #also check, if pak is installed
  if(!rlang::is_installed("impute")) {
    if(!rlang::is_installed("pak")) {
      rlang::check_installed("pak")
      rlang::check_installed("impute")
    }
    rlang::check_installed("impute")
  }
  data_colnames <- colnames(data)

  data_obs <- data %>%
    dplyr::select("UID", "Intensity", "Sample")

  #preserve order for later
  data_obs_sample_order <- data_obs %>%
    dplyr::select("Sample") %>%
    dplyr::pull() %>%
    unique()

  data_meta <- data %>%
    dplyr::select(-"Intensity")

  data_obs <- data_obs %>%
    dplyr::mutate(Intensity = log(.data$Intensity)) %>%
    tidyr::spread(key = "Sample", value = "Intensity")

  uids <- data_obs %>%
    dplyr::select("UID")

  data_obs <- data_obs %>%
    dplyr::select(-"UID")

  #preserve colnames
  data_obs_colnames <- colnames(data_obs)

  data_obs <- data_obs %>%
    as.matrix()


  #used with_preserve_seed to preserve random seed
  #write a test to check if it works (i.e., .Random.seed before == .Random.seed after)
  cat("Messages from impute.knn:\n")
  data_obs <- withr::with_preserve_seed(impute::impute.knn(data_obs, ...))

  data_obs <- data_obs$data %>%
    tidyr::as_tibble()

  #restore colnames
  colnames(data_obs) <- data_obs_colnames

  data_obs <- data_obs[data_obs_sample_order]

  #print(data_obs)
  data <- data_obs %>%
    cbind(uids) %>%
    tidyr::gather(-"UID", key = "Sample", value = "Intensity") %>%
    #reverse ln-transformation
    dplyr::mutate(Intensity = exp(.data$Intensity)) %>%
    dplyr::left_join(data_meta, by = c("UID", "Sample"))

  #bring columns to correct order
  data <- data[data_colnames]

  data
}

impute_rf <- function() {

}

impute_svd <- function() {

}

impute_qrilc <- function() {

}
