#' Convert a wide feature table to a tidy tibble
#'
#' @description
#' Feature tables are usually stored in a "wide" data format where sample names are stored in columns and features are stored in rows.
#' This functions transforms those feature tables into a "long" and tidy data format to use it with functions provided in the `metamorphr` package.
#' `convert_from_wide` works with tibbles and data frames. To convert a matrix, see \code{\link[metamorphr]{convert_from_matrix}}.
#'
#' @param data A feature table data frame or tibble in wide format. To convert a matrix, see \code{\link[metamorphr]{convert_from_matrix}}.
#' @param label_col The index or name of the column that will be used to label Features. For example an identifier (_e.g._, KEGG, CAS, HMDB) or a _m/z_-RT pair.
#' @param metadata_cols The index/indices or name(s) of column(s) that hold additional feature metadata (_e.g._, retention times, additional identifiers or _m/z_ values).
#'
#' @returns A tidy tibble.
#' @export
#'
#' @examples
#' featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
#' featuretable_wide <- read.csv(featuretable_path)
#'
#' convert_from_wide(featuretable_wide, metadata_cols = 2:5)
#'
convert_from_wide <- function(data, label_col = 1, metadata_cols = NULL) {
  # perform some checks
  if (length(label_col) > 1) {
    rlang::abort("label_col must be of length 1.")
  }

  if (!is.data.frame(data)) {
    if (!is.matrix(data)) {
      rlang::abort(paste0("`data` must be of class `tibble` or `data.frame`, not `", paste0(class(data), collapse = ", "), "`."))
    } else {
      rlang::abort("`data` must be of class `tibble` or `data.frame`, not `matrix`. To convert a matrix use function `convert_from_matrix`.")
    }
  }

  data_colnames <- colnames(data)

  # functionality for when label_col is a numeric
  if (is.numeric(label_col)) {
    label_col <- data_colnames[label_col]
  }

  # functionality for when metadata_cols is a numeric
  if (all(!(is.null(metadata_cols)), is.numeric(metadata_cols))) {
    metadata_cols <- data_colnames[metadata_cols]
  }

  # 1: always UID
  metadata_cols <- c("UID", metadata_cols, "Feature")

  metadata_cols <- unique(metadata_cols)

  # renamed Measurement -> Sample; label -> Feature
  data %>%
    tibble::as_tibble() %>%
    dplyr::rename("Feature" = dplyr::all_of(label_col)) %>%
    dplyr::mutate(Feature = as.character(.data$Feature)) %>%
    dplyr::mutate(UID = seq(1, length(.data$Feature))) %>%
    dplyr::relocate("UID", .before = 1) %>%
    dplyr::relocate("Feature", .after = 1) %>%
    tidyr::gather(-dplyr::any_of(metadata_cols), key = "Sample", value = "Intensity") %>%
    dplyr::relocate("Sample", .after = 2) %>%
    dplyr::relocate("Intensity", .after = 3) %>%
    dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    # replace 0 with NA
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0))
}

#'  Convert a wide matrix to a tidy tibble
#'
#' @description
#' This functions transforms a matrix holding a wide feature table into a "long" and tidy tibble to use it with functions provided in the `metamorphr` package.
#' `convert_from_matrix` works with objects of class `matrix`. To convert a data frame or tibble, see \code{\link[metamorphr]{convert_from_wide}}.
#'
#' @param data A feature table matrix in wide format. To convert a wide data frame, see \code{\link[metamorphr]{convert_from_wide}}.
#' @param samples_in_cols `TRUE` if samples are in columns and features in rows, `FALSE` if it is reversed. See examples for more information.
#'
#' @returns A tidy tibble.
#' @export
#'
#' @examples
#' # Using a small fictional data set
#' dataset <- matrix(1:9, ncol = 3)
#' colnames(dataset) <- paste0("sample", 1:3)
#' rownames(dataset) <- paste0("feature", 1:3)
#'
#'
#' # Example 1: Samples in columns
#' dataset
#' convert_from_matrix(dataset)
#'
#' # Example 2: Samples in rows
#' dataset_transposed <- t(dataset)
#' dataset_transposed
#'
#' convert_from_matrix(dataset, samples_in_cols = FALSE)
convert_from_matrix <- function(data, samples_in_cols = TRUE) {
  if (!is.matrix(data)) {
    if (!is.data.frame(data)) {
      rlang::abort(paste0("`data` must be of class `matrix`, not `", paste0(class(data), collapse = ", "), "`."))
    } else {
      rlang::abort("`data` must be of class `matrix`, not `data.frame`. To convert a data frame use function `convert_from_wide`.")
    }
  }

  if (samples_in_cols == FALSE) {
    data <- t(data)
  }

  data %>% tibble::as_tibble(rownames = "Feature") %>%
    convert_from_wide(label_col = "Feature")
}
