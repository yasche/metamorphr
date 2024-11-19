#' Read a feature table into a tidy tibble
#'
#' @description
#' Basically a wrapper around `readr::read_delim()` but performs some initial tidying operations such as `gather()` rearranging columns. The `label_col` will be renamed to _Feature_.
#'
#'
#' @param file A path to a file but can also be a connection or literal data.
#' @param delim The field separator or delimiter. For example "," in csv files.
#' @param label_col The index of the column that will be used to label Features. For example an identifier (_e.g._, KEGG, CAS, HMDB) or a _m/z_-RT pair.
#' @param metadata_cols The index/indices of column(s) that hold additional metadata (_e.g._, retention times, additional identifiers or _m/z_ values).
#' @param ... Additional arguments passed on to `readr::read_delim()`
#'
#' @return A tidy tibble.
#' @export
#'
#' @examples
#' featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
#' featuretable <- read_featuretable(featuretable_path, metadata_cols = 2:5)
read_featuretable <- function(file, delim = ",", label_col = 1, metadata_cols = NULL, ...) {
  # perform some checks
  if (length(label_col) > 1) {
    stop("label_col must be of length 1.")
  }

  # if (!is.null(drop_cols)) {
  #  if (label_col %in% drop_cols) {
  #    stop("label_col can not be dropped.")
  #  }
  # }

  # 1: always UID
  metadata_cols <- c(1, metadata_cols + 1, label_col + 1)

  metadata_cols <- unique(metadata_cols)
  # print(metadata_cols)

  # renamed Measurement -> Sample; label -> Feature
  data <- readr::read_delim(file = file, delim = delim, show_col_types = FALSE, ...) %>%
    dplyr::rename("Feature" = label_col) %>%
    # select(- {{ drop_cols }}) %>%
    dplyr::mutate(Feature = as.character(.data$Feature)) %>%
    dplyr::mutate(UID = seq(1, length(.data$Feature))) %>%
    dplyr::relocate("UID", .before = 1) %>%
    dplyr::relocate("Feature", .after = 1) %>%
    # print()
    tidyr::gather(-metadata_cols, key = "Sample", value = "Intensity") %>%
    dplyr::relocate("Sample", .after = 2) %>%
    dplyr::relocate("Intensity", .after = 3) %>%
    dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    # replace 0 with NA
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0))

  data
}


#' Create a blank metadata skeleton
#'
#' @description
#' The function takes a tidy tibble created by `metamorph::read_featuretable()` and returns an empty tibble for sample metadata. The tibble can either be populated directly in R or exported and edited by hand (_e.g._ in Excel). Metadata are necessary for several downstream functions. More columns may be added if necessary.
#'
#'
#' @param data A tidy tibble created by `metamorph::read_featuretable()`.
#'
#' @return An empty tibble structure with the necessary columns for metadata.
#' @export
#'
#' @examples featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
#' metadata <- read_featuretable(featuretable_path, metadata_cols = 2:5) %>%
#'   create_metadata_skeleton()
create_metadata_skeleton <- function(data) {
  sample_names <- data %>%
    dplyr::select("Sample") %>%
    dplyr::pull() %>%
    unique()

  metadata <- tibble::tibble(
    Sample = sample_names,
    Group = NA,
    Replicate = NA,
    Batch = NA,
    Factor = 1
  )

  metadata
}
