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
      .default = .data$Intensity
    )) %>%
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
      .default = .data$Intensity
    ))
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
      .default = .data$Intensity
    )) %>%
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
      .default = .data$Intensity
    )) %>%
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
      .default = .data$Intensity
    )) %>%
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
  # https://omicsforum.ca/t/how-to-deal-with-missing-values/75
  data %>%
    dplyr::group_by(.data$UID) %>%
    dplyr::mutate(LoD = min(.data$Intensity, na.rm = T) / .env$div_by) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Intensity = dplyr::case_when(is.na(.data$Intensity) ~ .data$LoD,
      .default = .data$Intensity
    )) %>%
    dplyr::select(-"LoD")
}

#' Impute missing values using nearest neighbor averaging
#'
#' @description
#' Basically a wrapper function around `impute::`\code{\link[impute]{impute.knn}}. Imputes missing values using the k-th nearest neighbor algorithm.
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
#' @param quietly `TRUE` or `FALSE`. Should messages and warnings from \code{\link[impute]{impute.knn}} be printed to the console?
#' @param ... Additional parameters passed to \code{\link[impute]{impute.knn}}.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>Robert Tibshirani, Trevor Hastie, <b>2017</b>, DOI <a href = "https://doi.org/10.18129/B9.BIOC.IMPUTE">10.18129/B9.BIOC.IMPUTE</a>.</li>
#' <li>J. Khan, J. S. Wei, M. Ringnér, L. H. Saal, M. Ladanyi, F. Westermann, F. Berthold, M. Schwab, C. R. Antonescu, C. Peterson, P. S. Meltzer, <i>Nat Med</i> <b>2001</b>, <i>7</i>, 673–679, DOI <a href = "https://doi.org/10.1038/89044">10.1038/89044</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_knn()
impute_knn <- function(data, quietly = TRUE, ...) {
  # impute is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  # also check, if pak is installed
  if (!is_installed_wrapper("impute")) {
    if (!is_installed_wrapper("pak")) {
      check_installed_wrapper("pak")
      check_installed_wrapper("impute")
    }
    check_installed_wrapper("impute")
  }
  # to preserve order
  data_colnames <- colnames(data)

  data_obs <- data %>%
    dplyr::select("UID", "Intensity", "Sample")

  # preserve order for later
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

  # preserve colnames
  data_obs_colnames <- colnames(data_obs)

  data_obs <- data_obs %>%
    as.matrix()

  # used with_preserve_seed to preserve random seed
  if (quietly == TRUE) {
    data_obs <- knn_impute_quiet(data_obs, ...)
    # warnings will be printed anyways
    if (length(data_obs$warnings) > 0) {
      warning(data_obs$warnings)
    }
    data_obs <- data_obs$result
  } else {
    data_obs <- withr::with_preserve_seed(impute::impute.knn(data_obs, ...))
  }


  data_obs <- data_obs$data %>%
    tidyr::as_tibble()

  # restore colnames
  colnames(data_obs) <- data_obs_colnames

  data_obs <- data_obs[data_obs_sample_order]

  # print(data_obs)
  data <- data_obs %>%
    cbind(uids) %>%
    tidyr::gather(-"UID", key = "Sample", value = "Intensity") %>%
    # reverse ln-transformation
    dplyr::mutate(Intensity = exp(.data$Intensity)) %>%
    dplyr::left_join(data_meta, by = c("UID", "Sample"))

  # bring columns to correct order
  data <- data[data_colnames]

  data <- tidyr::as_tibble(data)


  data
}

#' Impute missing values using random forest
#'
#' @description
#' Basically a wrapper function around `missForest::`\code{\link[missForest]{missForest}}. Imputes missing values using the random forest algorithm.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param random_seed A seed for the random number generator. Can be an integer or `NULL` (in case no particular seed should be used) but for reproducibility reasons it is **strongly advised** to provide an integer.
#' @param ... Additional parameters passed to \code{\link[missForest]{missForest}}.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>Daniel J. Stekhoven <stekhoven@stat.math.ethz.ch>, <b>2011</b>, DOI <a href = "https://doi.org/10.32614/CRAN.package.missForest">10.32614/CRAN.package.missForest</a>.</li>
#' <li>D. J. Stekhoven, P. Bühlmann, <i>Bioinformatics</i> <b>2012</b>, <i>28</i>, 112–118, DOI <a href = "https://doi.org/10.1093/bioinformatics/btr597">10.1093/bioinformatics/btr597</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_rf()
impute_rf <- function(data, random_seed = 1L, ...) {
  # to preserve order of columns of initial tibble
  data_colnames <- colnames(data)

  data_obs <- data %>%
    dplyr::select("UID", "Intensity", "Sample")

  # preserve order for later
  data_obs_sample_order <- data_obs %>%
    dplyr::select("Sample") %>%
    dplyr::pull() %>%
    unique()

  data_meta <- data %>%
    dplyr::select(-"Intensity")

  data_obs <- data_obs %>%
    tidyr::spread(key = "UID", value = "Intensity") %>%
    as.data.frame()

  # preserve rownames (Sample names)
  data_obs_rownames <- data_obs %>%
    dplyr::select("Sample")

  data_obs <- dplyr::select(data_obs, -"Sample")


  if (!is.null(random_seed)) {
    data_obs_imp <- withr::with_seed(seed = random_seed, missForest::missForest(data_obs, ...))
  } else {
    # with_preserve_seed might not be necessary
    data_obs_imp <- withr::with_preserve_seed(missForest::missForest(data_obs, ...))
  }

  data_obs_imp <- data_obs_imp$ximp

  data_obs_imp <- data_obs_imp %>%
    cbind(data_obs_rownames) %>%
    tidyr::gather(-"Sample", key = "UID", value = "Intensity") %>%
    dplyr::mutate(UID = as.integer(.data$UID))

  data_obs_imp <- dplyr::left_join(data_meta, data_obs_imp, by = c("UID", "Sample")) %>%
    tidyr::as_tibble()

  data_obs_imp <- data_obs_imp[data_colnames]

  data_obs_imp
}

#' Impute missing values using NIPALS PCA
#'
#' @description
#' One of several PCA-based imputation methods. Basically a wrapper around `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "nipals")`.
#' For a detailed discussion, see the `vignette("pcaMethods")` and `vignette("missingValues", "pcaMethods")` as well as the References section.
#'
#' <b>Important Note</b><br>
#' `impute_nipals()` depends on the `pcaMethods` package from Bioconductor. If `metamorphr` was installed via `install.packages()`, dependencies from Bioconductor were not
#' automatically installed. When `impute_nipals()` is called without the `pcaMethods` package installed, you should be asked if you want to install `pak` and `pcaMethods`.
#' If you want to use `impute_nipals()` you have to install those. In case you run into trouble with the automatic installation, please install `pcaMethods` manually. See
#' <a href = "https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html">pcaMethods – a Bioconductor package providing PCA methods for incomplete data.</a> for instructions on manual installation.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_pcs The number of PCs to calculate.
#' @param center Should `data` be mean centered? See \code{\link[pcaMethods]{prep}} for details.
#' @param scale Should `data` be scaled? See \code{\link[pcaMethods]{prep}} for details.
#' @param direction Either `1` or `2`. `1` runs a PCA on a matrix with samples in columns and features in rows and `2` runs a PCA on a matrix with features in columns and samples in rows.
#' Both are valid according to this <a href = "https://github.com/hredestig/pcaMethods/issues/25">discussion on GitHub</a> but give <b>different results</b>.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>H. R. Wolfram Stacklies, <b>2017</b>, DOI <a href = "https://doi.org/10.18129/B9.BIOC.PCAMETHODS">10.18129/B9.BIOC.PCAMETHODS</a>.</li>
#' <li>W. Stacklies, H. Redestig, M. Scholz, D. Walther, J. Selbig, <i>Bioinformatics</i> <b>2007</b>, <i>23</i>, 1164–1167, DOI <a href = "https://doi.org/10.1093/bioinformatics/btm069">10.1093/bioinformatics/btm069</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_nipals()
impute_nipals <- function(data, n_pcs = 2, center = TRUE, scale = "none", direction = 2) {
  # pcaMethods is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  # also check, if pak is installed
  if (!is_installed_wrapper("pcaMethods")) {
    if (!is_installed_wrapper("pak")) {
      check_installed_wrapper("pak")
      check_installed_wrapper("pcaMethods")
    }
    check_installed_wrapper("pcaMethods")
  }

  data_list <- internal_prep_pca_imputes(data = data, direction = direction)

  #data <- data_list$data


  data_list$data <- pcaMethods::pca(data_list$data, nPcs = n_pcs, method = "nipals")
  data_list$data <- pcaMethods::completeObs(data_list$data)

  internal_clean_pca_results(data_list = data_list, direction = direction)
}

#' Impute missing values using Bayesian PCA
#'
#'
#' @description
#' One of several PCA-based imputation methods. Basically a wrapper around `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "bpca")`.
#' For a detailed discussion, see the `vignette("pcaMethods")` and `vignette("missingValues", "pcaMethods")` as well as the References section.
#'
#' <b>Important Note</b><br>
#' `impute_bpca()` depends on the `pcaMethods` package from Bioconductor. If `metamorphr` was installed via `install.packages()`, dependencies from Bioconductor were not
#' automatically installed. When `impute_bpca()` is called without the `pcaMethods` package installed, you should be asked if you want to install `pak` and `pcaMethods`.
#' If you want to use `impute_bpca()` you have to install those. In case you run into trouble with the automatic installation, please install `pcaMethods` manually. See
#' <a href = "https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html">pcaMethods – a Bioconductor package providing PCA methods for incomplete data.</a> for instructions on manual installation.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_pcs The number of PCs to calculate.
#' @param center Should `data` be mean centered? See \code{\link[pcaMethods]{prep}} for details.
#' @param scale Should `data` be scaled? See \code{\link[pcaMethods]{prep}} for details.
#' @param direction Either `1` or `2`. `1` runs a PCA on a matrix with samples in columns and features in rows and `2` runs a PCA on a matrix with features in columns and samples in rows.
#' Both are valid according to this <a href = "https://github.com/hredestig/pcaMethods/issues/25">discussion on GitHub</a> but give <b>different results</b>.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>H. R. Wolfram Stacklies, <b>2017</b>, DOI <a href = "https://doi.org/10.18129/B9.BIOC.PCAMETHODS">10.18129/B9.BIOC.PCAMETHODS</a>.</li>
#' <li>W. Stacklies, H. Redestig, M. Scholz, D. Walther, J. Selbig, <i>Bioinformatics</i> <b>2007</b>, <i>23</i>, 1164–1167, DOI <a href = "https://doi.org/10.1093/bioinformatics/btm069">10.1093/bioinformatics/btm069</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_bpca()
impute_bpca <- function(data, n_pcs = 2, center = TRUE, scale = "none", direction = 2) {
  # pcaMethods is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  # also check, if pak is installed
  if (!is_installed_wrapper("pcaMethods")) {
    if (!is_installed_wrapper("pak")) {
      check_installed_wrapper("pak")
      check_installed_wrapper("pcaMethods")
    }
    check_installed_wrapper("pcaMethods")
  }

  data_list <- internal_prep_pca_imputes(data = data, direction = direction)

  #data <- data_list$data

  cat("`impute_bpca` output:\n\n")
  data_list$data <- pcaMethods::pca(data_list$data, nPcs = n_pcs, method = "bpca")
  data_list$data <- pcaMethods::completeObs(data_list$data)

  internal_clean_pca_results(data_list = data_list, direction = direction)
}

#' Impute missing values using Probabilistic PCA
#'
#'
#' @description
#' One of several PCA-based imputation methods. Basically a wrapper around `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "ppca")`.
#' For a detailed discussion, see the `vignette("pcaMethods")` and `vignette("missingValues", "pcaMethods")` as well as the References section. <br>
#' In the underlying function (`pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "ppca")`), the order of columns has an influence on the outcome. Therefore, calling `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "ppca")`
#' on a matrix and calling `metamorphr::impute()` on a tidy tibble might give different results, even though they contain the same data. That is because under the hood,
#' the tibble is transformed to a matrix prior to calling `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "ppca")` and you have limited influence on the column order of the
#' resulting matrix.
#'
#' <b>Important Note</b><br>
#' `impute_ppca()` depends on the `pcaMethods` package from Bioconductor. If `metamorphr` was installed via `install.packages()`, dependencies from Bioconductor were not
#' automatically installed. When `impute_ppca()` is called without the `pcaMethods` package installed, you should be asked if you want to install `pak` and `pcaMethods`.
#' If you want to use `impute_ppca()` you have to install those. In case you run into trouble with the automatic installation, please install `pcaMethods` manually. See
#' <a href = "https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html">pcaMethods – a Bioconductor package providing PCA methods for incomplete data.</a> for instructions on manual installation.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_pcs The number of PCs to calculate.
#' @param center Should `data` be mean centered? See \code{\link[pcaMethods]{prep}} for details.
#' @param scale Should `data` be scaled? See \code{\link[pcaMethods]{prep}} for details.
#' @param direction Either `1` or `2`. `1` runs a PCA on a matrix with samples in columns and features in rows and `2` runs a PCA on a matrix with features in columns and samples in rows.
#' Both are valid according to this <a href = "https://github.com/hredestig/pcaMethods/issues/25">discussion on GitHub</a> but give <b>different results</b>.
#' @param random_seed An integer used as seed for the random number generator.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>H. R. Wolfram Stacklies, <b>2017</b>, DOI <a href = "https://doi.org/10.18129/B9.BIOC.PCAMETHODS">10.18129/B9.BIOC.PCAMETHODS</a>.</li>
#' <li>W. Stacklies, H. Redestig, M. Scholz, D. Walther, J. Selbig, <i>Bioinformatics</i> <b>2007</b>, <i>23</i>, 1164–1167, DOI <a href = "https://doi.org/10.1093/bioinformatics/btm069">10.1093/bioinformatics/btm069</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_ppca()
impute_ppca <- function(data, n_pcs = 2, center = TRUE, scale = "none", direction = 2, random_seed = 1L) {
  # pcaMethods is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  # also check, if pak is installed
  if (!is_installed_wrapper("pcaMethods")) {
    if (!is_installed_wrapper("pak")) {
      check_installed_wrapper("pak")
      check_installed_wrapper("pcaMethods")
    }
    check_installed_wrapper("pcaMethods")
  }

  data_list <- internal_prep_pca_imputes(data = data, direction = direction)

  #data <- data_list$data

  #data_list$data <- withr::with_seed(seed = random_seed, pcaMethods::pca(data_list$data, nPcs = n_pcs, method = "ppca"))
  data_list$data <- withr::with_preserve_seed(pcaMethods::pca(data_list$data, nPcs = n_pcs, method = "ppca", seed = random_seed))
  data_list$data <- pcaMethods::completeObs(data_list$data)

  internal_clean_pca_results(data_list = data_list, direction = direction)
}

#' Impute missing values using Singular Value Decomposition (SVD)
#'
#'
#' @description
#' Basically a wrapper around `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "svdImpute")`.
#' For a detailed discussion, see the `vignette("pcaMethods")` and `vignette("missingValues", "pcaMethods")` as well as the References section.
#'
#' <b>Important Note</b><br>
#' `impute_svd()` depends on the `pcaMethods` package from Bioconductor. If `metamorphr` was installed via `install.packages()`, dependencies from Bioconductor were not
#' automatically installed. When `impute_svd()` is called without the `pcaMethods` package installed, you should be asked if you want to install `pak` and `pcaMethods`.
#' If you want to use `impute_svd()` you have to install those. In case you run into trouble with the automatic installation, please install `pcaMethods` manually. See
#' <a href = "https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html">pcaMethods – a Bioconductor package providing PCA methods for incomplete data.</a> for instructions on manual installation.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_pcs The number of PCs to calculate.
#' @param center Should `data` be mean centered? See \code{\link[pcaMethods]{prep}} for details.
#' @param scale Should `data` be scaled? See \code{\link[pcaMethods]{prep}} for details.
#' @param direction Either `1` or `2`. `1` runs `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "svdImpute")` on a matrix with samples in columns and features in rows and `2` runs `pcaMethods::`\code{\link[pcaMethods]{pca}}`(method = "svdImpute")` on a matrix with features in columns and samples in rows.
#' Both are valid according to this <a href = "https://github.com/hredestig/pcaMethods/issues/25">discussion on GitHub</a> but give <b>different results</b>.
#'
#' @return A tibble with imputed missing values.
#' @export
#'
#' @references For further information, see
#' <ul>
#' <li>H. R. Wolfram Stacklies, <b>2017</b>, DOI <a href = "https://doi.org/10.18129/B9.BIOC.PCAMETHODS">10.18129/B9.BIOC.PCAMETHODS</a>.</li>
#' <li>W. Stacklies, H. Redestig, M. Scholz, D. Walther, J. Selbig, <i>Bioinformatics</i> <b>2007</b>, <i>23</i>, 1164–1167, DOI <a href = "https://doi.org/10.1093/bioinformatics/btm069">10.1093/bioinformatics/btm069</a>.</li>
#' <li>O. Troyanskaya, M. Cantor, G. Sherlock, P. Brown, T. Hastie, R. Tibshirani, D. Botstein, R. B. Altman, <i>Bioinformatics</i> <b>2001</b>, <i>17</i>, 520–525, DOI <a href = "https://doi.org/10.1093/bioinformatics/17.6.520">10.1093/bioinformatics/17.6.520</a>.</li>
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_svd()
impute_svd <- function(data, n_pcs = 2, center = TRUE, scale = "none", direction = 2) {
  # pcaMethods is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  # also check, if pak is installed
  if (!is_installed_wrapper("pcaMethods")) {
    if (!is_installed_wrapper("pak")) {
      check_installed_wrapper("pak")
      check_installed_wrapper("pcaMethods")
    }
    check_installed_wrapper("pcaMethods")
  }

  data_list <- internal_prep_pca_imputes(data = data, direction = direction)

  #data <- data_list$data

  #data_list$data <- withr::with_seed(seed = random_seed, pcaMethods::pca(data_list$data, nPcs = n_pcs, method = "ppca"))
  data_list$data <- pcaMethods::pca(data_list$data, nPcs = n_pcs, method = "svdImpute", verbose = F)
  data_list$data <- pcaMethods::completeObs(data_list$data)

  internal_clean_pca_results(data_list = data_list, direction = direction)
}

impute_nlpca <- function(data, n_pcs = 2, center = TRUE, scale = "none", direction = 2) {

}

impute_lls <- function(data, n_pcs = 2, center = TRUE, scale = "none", direction = 2) {

}

impute_qrilc <- function() {

}
