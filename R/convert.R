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

  # functionality for when label_col is a character
  if (is.character(label_col)) {
    label_col <- which(data_colnames == label_col)
  }

  # functionality for when metadata_cols is a character
  if (all(!(is.null(metadata_cols)), is.character(metadata_cols))) {
    metadata_cols <- which(data_colnames %in% metadata_cols)
  }

  # 1: always UID
  metadata_cols <- c(1, metadata_cols + 1, label_col + 1)

  metadata_cols <- unique(metadata_cols)

  # renamed Measurement -> Sample; label -> Feature
  data %>%
    tibble::as_tibble() %>%
    dplyr::rename("Feature" = dplyr::all_of(label_col)) %>%
    # select(- {{ drop_cols }}) %>%
    dplyr::mutate(Feature = as.character(.data$Feature)) %>%
    dplyr::mutate(UID = seq(1, length(.data$Feature))) %>%
    dplyr::relocate("UID", .before = 1) %>%
    dplyr::relocate("Feature", .after = 1) %>%
    # print()
    tidyr::gather(-dplyr::all_of(metadata_cols), key = "Sample", value = "Intensity") %>%
    dplyr::relocate("Sample", .after = 2) %>%
    dplyr::relocate("Intensity", .after = 3) %>%
    dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    # replace 0 with NA
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0))
}

convert_from_matrix <- function(data, samples_in_cols = TRUE) {

}
