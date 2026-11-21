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

#' Convert a tidy tibble into a format compatible with the "Functional Analysis" module in 'MetaboAnalyst'
#'
#' @description
#' Creates a tibble with columns `m.z`, `p.value`, `t.score` and optionally
#' `r.t` to use in the "Functional Analysis" module in 'MetaboAnalyst'.
#' Metrics are calculated using a \code{\link[stats]{t.test}}.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param m_z_col Which column holds the precursor m/z? Uses \code{\link[rlang]{args_data_masking}}.
#' @param rt_col Optional. Which column holds the precursor retention times? Uses \code{\link[rlang]{args_data_masking}}.
#' @param group_column Which column should be used for grouping? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param groups_to_compare Names of the groups which should be compared as a character vector. Those are the group names in the `group_column`. They are usually provided in the form of a metadata tibble and joined via \code{\link[metamorphr]{join_metadata}}.
#' @param batch_column Which column contains the batch information? Usually `batch_column = Batch`. Only relevant if `data` contains multiple batches. For example, if `data` contains 2 batches and each batch contains measurements of separate controls, `group_column` and `batch` arguments should be provided. Otherwise controls of both batches will be considered when calculating the \emph{p}-value and log2 fold change. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch The names of the batch(es) that should be included when calculating \emph{p}-value and log2 fold change.
#' @param adjust_p Should the \emph{p}-value be adjusted? Can be either `FALSE`, (the default) in case no adjustment should be made or any or the name from \code{\link[stats]{p.adjust.methods}} (e.g., `adjust_p = "fdr"`).
#' @param log2_before A logical. Should the data be log2 transformed prior to calculating the \emph{p}-values?
#' @param ... Arguments passed on to \code{\link[stats]{t.test}}. If none are provided (the default), a Welch Two Sample \emph{t}-test will be performed.
#'
#' @returns A tibble with columns `m.z`, `p.value`, `t.score` and optionally
#' `r.t`.
#'
#' @export
#'
#' @examples
#' # Without optional retention time column
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   convert_to_fa(group_column = Group,
#'                 groups_to_compare = c("control", "treatment"),
#'                 return_tbl = TRUE,
#'                 alternative = "two.sided",
#'                 m_z_col = `m/z`)
#'
#' # With optional retention time column
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   convert_to_fa(group_column = Group,
#'                 groups_to_compare = c("control", "treatment"),
#'                 return_tbl = TRUE,
#'                 m_z_col = `m/z`,
#'                 rt_col = RT)
convert_to_fa <- function(data, m_z_col, rt_col = NULL, group_column, groups_to_compare, batch_column = NULL, batch = NULL, adjust_p = FALSE, log2_before = FALSE, ...) {
  batch_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(batch_column))))
  if (batch_column_string != "NULL") {
    data <- data %>%
      dplyr::filter({{ batch_column }} %in% batch)
  }

  if (nrow(data) == 0) {
    data <- dplyr::select(data, {{ rt_col }})

    data <- tibble::tibble(
      m.z = numeric(),
      p.value = numeric(),
      t.score = numeric()
    ) %>%
      dplyr::bind_cols(data) %>%
      dplyr::rename("r.t" = {{ rt_col }})



    return(data)
  }

  feature_metadata <- data %>%
    dplyr::select("UID",
                  m.z = {{ m_z_col }},
                  r.t = {{ rt_col }}) %>%
    dplyr::distinct()

  data <- internal_t_test_tibble(data = data,
                         group_column = {{ group_column }},
                         name_column = NULL,
                         groups_to_compare = groups_to_compare,
                         adjust_p = adjust_p,
                         log2_before = log2_before,
                         ...)


  data %>%
    dplyr::left_join(feature_metadata, by = "UID") %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c("m.z",
                                  "p.value",
                                  "statistic",
                                  "r.t"))) %>%
    dplyr::rename("t.score" = "statistic") %>%
    dplyr::arrange("m.z")
}
