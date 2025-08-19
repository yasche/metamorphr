collapse_helper <- function(collapse_fn, collapse_fn_string, data, feature_metadata_cols, sample_metadata_cols, separator, group_column, replicate_column, batch_column, group_column_string, replicate_column_string, batch_column_string) {
  # rlang::as_string(rlang::enquo(batch_column))



  col_order <- colnames(data)
  # define the cols to keep in returned data frame.
  # by default, those are the feature meta data UID, Feature, Intensity, group_column. replicate_column and the columns defined in feature_metadata_cols
  # those are needed to restore column order later
  # cols_to_keep <- c("UID",
  #                  "Intensity",
  #                  "Sample",
  #                  group_column_string,
  #                  replicate_column_string,
  #                  batch_column_string,
  #                  feature_metadata_cols)#

  # if (!is.null(sample_metadata_cols)) {
  #  cols_to_keep <- c(cols_to_keep, sample_metadata_cols)
  # }

  # cols_to_keep <- colnames(data)[colnames(data) %in% cols_to_keep]

  # feature metadata to join back with the original df later
  feature_metadata <- dplyr::select(data, "UID", dplyr::all_of(feature_metadata_cols)) %>%
    dplyr::distinct()


  # sample metadata to join back with the original df later
  sample_metadata <- dplyr::select(data, dplyr::all_of(c(group_column_string, replicate_column_string, batch_column_string)), dplyr::all_of(sample_metadata_cols)) %>%
    dplyr::distinct()


  data <- data %>%
    dplyr::group_by(.data$UID, {{ group_column }}, {{ replicate_column }}, {{ batch_column }}) %>%
    dplyr::summarise(Intensity = collapse_fn(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup()

  new_sample_names <- dplyr::pull(data, {{ group_column }})

  batches <- dplyr::pull(data, {{ batch_column }})

  # if all samples belong to the same batch, the batch is left out of the new name
  if (length(unique(batches)) > 1) {
    new_sample_names <- paste0(
      new_sample_names,
      separator,
      dplyr::pull(data, {{ batch_column }}),
      separator,
      dplyr::pull(data, {{ replicate_column }})
    )
  } else {
    new_sample_names <- paste0(
      new_sample_names,
      separator,
      dplyr::pull(data, {{ replicate_column }})
    )
  }

  data$Sample <- new_sample_names


  # sanity check: are feature and sample metadata correctly assigned?
  length_sample_metadata <- nrow(sample_metadata)
  length_sample_metadata_expected <- data %>%
    dplyr::select("Sample") %>%
    dplyr::distinct() %>%
    nrow()

  if (!length_sample_metadata == length_sample_metadata_expected) {
    print(sample_metadata, n = nrow(sample_metadata))
    rlang::abort(paste0("\nThere is a problem in the 'sample_metadata_cols' argument you have provided.
Some observations in the columns you specified in 'sample_metadata_cols' (", paste(sample_metadata_cols, collapse = ", "), ") are not unique for a given ", group_column_string, " ", replicate_column_string, batch_column_string, " combination.
Each combination of ", group_column_string, ", ", replicate_column_string, "and", batch_column_string, " must only appear once in the table above.\n
Did you provide feature metadata as sample metadata? See ?collapse_", collapse_fn_string, " for more information."))
  }

  length_feature_metadata <- nrow(feature_metadata)
  length_feature_metadata_expected <- data %>%
    dplyr::select("UID") %>%
    dplyr::distinct() %>%
    nrow()

  if (!length_feature_metadata == length_feature_metadata_expected) {
    print(feature_metadata, n = nrow(feature_metadata))
    rlang::abort(paste0("\nThere is a problem in the 'feature_metadata_cols' argument you have provided.
Some observations in the columns you specified in 'feature_metadata_cols' (", paste(feature_metadata_cols, collapse = ", "), ") are not unique for a given UID.
Each UID must only appear once in the table above.\n
Did you provide sample metadata as feature metadata? See ?collapse_", collapse_fn_string, " for more information."))
  }

  data <- data %>%
    dplyr::left_join(feature_metadata, by = "UID") %>%
    dplyr::left_join(sample_metadata, by = c(group_column_string, replicate_column_string, batch_column_string))

  col_order <- col_order[col_order %in% colnames(data)]

  data %>%
    dplyr::relocate(dplyr::all_of(col_order))
}

#' Collapse intensities of technical replicates by calculating their mean
#'
#' @description
#' Calculates the mean of the intensity of technical replicates (e.g., if the same sample was injected multiple times or if multiple workups have been performed on the same starting material).
#' The function assigns new sample names by joining either group and replicate name, or if a batch column is specified group, replicate and batch together with a specified separator.
#' Due to the nature of the function, sample and feature metadata columns will be dropped unless they are specified with the according arguments.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param replicate_column Which column contains replicate information? Usually `replicate_column = Replicate`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch_column Which column contains batch information? If all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`) it will have no effect on the calculation. Usually `batch_column = Batch`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param feature_metadata_cols A character or character vector containing the names of the feature metadata columns. They are usually created when reading the feature table with \code{\link[metamorphr]{read_featuretable}}. Feature metadata columns not specified here will be dropped.
#' @param sample_metadata_cols A character or character vector containing the names of the sample metadata columns. They are usually created when joining the metadata with \code{\link[metamorphr]{join_metadata}}. Sample metadata columns not specified here will be dropped, except for `group_column`, `replicate_column` and `batch_column` if specified.
#' @param separator Separator used for joining group and replicate, or group, batch and replicate together to create the new sample names. The new sample names will be Group name, separator, Batch name, separator, Replicate name, or Group name, separator, Replicate name, in case all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`).
#'
#' @return A tibble with intensities of technical replicates collapsed.
#' @export
#'
#' @examples
#' # uses a slightly modified version of toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata$Replicate <- 1
#'
#' toy_metaboscape %>%
#'   join_metadata(collapse_toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   collapse_mean(group_column = Group, replicate_column = Replicate)
#'
collapse_mean <- function(data, group_column = .data$Group, replicate_column = .data$Replicate, batch_column = .data$Batch, feature_metadata_cols = "Feature", sample_metadata_cols = NULL, separator = "_") {
  # create a string from columns;
  # this approach is probably far from best practice but using .data is deprecated in tidyselect
  # (See https://www.tidyverse.org/blog/2022/10/tidyselect-1-2-0/) and I need a way to select() columns.
  #group_column_string <- gsub("\\.data\\$", "", gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(group_column)))))
  #replicate_column_string <- gsub("\\.data\\$", "", gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(replicate_column)))))
  #batch_column_string <- gsub("\\.data\\$", "", gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(batch_column)))))

  group_column_string <- rlang::as_label(rlang::enquo(group_column))
  replicate_column_string <- rlang::as_label(rlang::enquo(replicate_column))
  batch_column_string <- rlang::as_label(rlang::enquo(batch_column))
  collapse_helper(collapse_fn = mean, collapse_fn_string = "mean", data = data, feature_metadata_cols, sample_metadata_cols, separator, group_column = {{ group_column }}, replicate_column = {{ replicate_column }}, batch_column = {{ batch_column }}, group_column_string = group_column_string, replicate_column_string = replicate_column_string, batch_column_string = batch_column_string)
}


#' Collapse intensities of technical replicates by calculating their median
#'
#' @description
#' Calculates the median of the intensity of technical replicates (e.g., if the same sample was injected multiple times or if multiple workups have been performed on the same starting material).
#' The function assigns new sample names by joining either group and replicate name, or if a batch column is specified group, replicate and batch together with a specified separator.
#' Due to the nature of the function, sample and feature metadata columns will be dropped unless they are specified with the according arguments.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param replicate_column Which column contains replicate information? Usually `replicate_column = Replicate`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch_column Which column contains batch information? If all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`) it will have no effect on the calculation. Usually `batch_column = Batch`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param feature_metadata_cols A character or character vector containing the names of the feature metadata columns. They are usually created when reading the feature table with \code{\link[metamorphr]{read_featuretable}}. Feature metadata columns not specified here will be dropped.
#' @param sample_metadata_cols A character or character vector containing the names of the sample metadata columns. They are usually created when joining the metadata with \code{\link[metamorphr]{join_metadata}}. Sample metadata columns not specified here will be dropped, except for `group_column`, `replicate_column` and `batch_column` if specified.
#' @param separator Separator used for joining group and replicate, or group, batch and replicate together to create the new sample names. The new sample names will be Group name, separator, Batch name, separator, Replicate name, or Group name, separator, Replicate name, in case all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`).
#'
#' @return A tibble with intensities of technical replicates collapsed.
#' @export
#'
#' @examples
#' # uses a slightly modified version of toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata$Replicate <- 1
#'
#' toy_metaboscape %>%
#'   join_metadata(collapse_toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   collapse_median(group_column = Group, replicate_column = Replicate)
#'
collapse_median <- function(data, group_column = .data$Group, replicate_column = .data$Replicate, batch_column = .data$Batch, feature_metadata_cols = "Feature", sample_metadata_cols = NULL, separator = "_") {
  # See explanation in collapse_mean
  group_column_string <- rlang::as_label(rlang::enquo(group_column))
  replicate_column_string <- rlang::as_label(rlang::enquo(replicate_column))
  batch_column_string <- rlang::as_label(rlang::enquo(batch_column))

  collapse_helper(collapse_fn = stats::median, collapse_fn_string = "median", data = data, feature_metadata_cols, sample_metadata_cols, separator, group_column = {{ group_column }}, replicate_column = {{ replicate_column }}, batch_column = {{ batch_column }}, group_column_string = group_column_string, replicate_column_string = replicate_column_string, batch_column_string = batch_column_string)
}


#' Collapse intensities of technical replicates by calculating their minimum
#'
#' @description
#' Calculates the minimum of the intensity of technical replicates (e.g., if the same sample was injected multiple times or if multiple workups have been performed on the same starting material).
#' The function assigns new sample names by joining either group and replicate name, or if a batch column is specified group, replicate and batch together with a specified separator.
#' Due to the nature of the function, sample and feature metadata columns will be dropped unless they are specified with the according arguments.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param replicate_column Which column contains replicate information? Usually `replicate_column = Replicate`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch_column Which column contains batch information? If all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`) it will have no effect on the calculation. Usually `batch_column = Batch`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param feature_metadata_cols A character or character vector containing the names of the feature metadata columns. They are usually created when reading the feature table with \code{\link[metamorphr]{read_featuretable}}. Feature metadata columns not specified here will be dropped.
#' @param sample_metadata_cols A character or character vector containing the names of the sample metadata columns. They are usually created when joining the metadata with \code{\link[metamorphr]{join_metadata}}. Sample metadata columns not specified here will be dropped, except for `group_column`, `replicate_column` and `batch_column` if specified.
#' @param separator Separator used for joining group and replicate, or group, batch and replicate together to create the new sample names. The new sample names will be Group name, separator, Batch name, separator, Replicate name, or Group name, separator, Replicate name, in case all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`).
#'
#' @return A tibble with intensities of technical replicates collapsed.
#' @export
#'
#' @examples
#' # uses a slightly modified version of toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata$Replicate <- 1
#'
#' toy_metaboscape %>%
#'   join_metadata(collapse_toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   collapse_min(group_column = Group, replicate_column = Replicate)
#'
collapse_min <- function(data, group_column = .data$Group, replicate_column = .data$Replicate, batch_column = .data$Batch, feature_metadata_cols = "Feature", sample_metadata_cols = NULL, separator = "_") {
  # See explanation in collapse_mean
  group_column_string <- rlang::as_label(rlang::enquo(group_column))
  replicate_column_string <- rlang::as_label(rlang::enquo(replicate_column))
  batch_column_string <- rlang::as_label(rlang::enquo(batch_column))

  collapse_helper(collapse_fn = min, collapse_fn_string = "min", data = data, feature_metadata_cols, sample_metadata_cols, separator, group_column = {{ group_column }}, replicate_column = {{ replicate_column }}, batch_column = {{ batch_column }}, group_column_string = group_column_string, replicate_column_string = replicate_column_string, batch_column_string = batch_column_string)
}


#' Collapse intensities of technical replicates by calculating their maximum
#'
#' @description
#' Calculates the minimum of the intensity of technical replicates (e.g., if the same sample was injected multiple times or if multiple workups have been performed on the same starting material).
#' The function assigns new sample names by joining either group and replicate name, or if a batch column is specified group, replicate and batch together with a specified separator.
#' Due to the nature of the function, sample and feature metadata columns will be dropped unless they are specified with the according arguments.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param replicate_column Which column contains replicate information? Usually `replicate_column = Replicate`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch_column Which column contains batch information? If all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`) it will have no effect on the calculation. Usually `batch_column = Batch`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param feature_metadata_cols A character or character vector containing the names of the feature metadata columns. They are usually created when reading the feature table with \code{\link[metamorphr]{read_featuretable}}. Feature metadata columns not specified here will be dropped.
#' @param sample_metadata_cols A character or character vector containing the names of the sample metadata columns. They are usually created when joining the metadata with \code{\link[metamorphr]{join_metadata}}. Sample metadata columns not specified here will be dropped, except for `group_column`, `replicate_column` and `batch_column` if specified.
#' @param separator Separator used for joining group and replicate, or group, batch and replicate together to create the new sample names. The new sample names will be Group name, separator, Batch name, separator, Replicate name, or Group name, separator, Replicate name, in case all samples belong to the same batch (i.e., they all have the same batch identifier in the `batch_column`).
#'
#' @return A tibble with intensities of technical replicates collapsed.
#' @export
#'
#' @examples
#' # uses a slightly modified version of toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata$Replicate <- 1
#'
#' toy_metaboscape %>%
#'   join_metadata(collapse_toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   collapse_max(group_column = Group, replicate_column = Replicate)
#'
collapse_max <- function(data, group_column = .data$Group, replicate_column = .data$Replicate, batch_column = .data$Batch, feature_metadata_cols = "Feature", sample_metadata_cols = NULL, separator = "_") {
  # See explanation in collapse_mean
  group_column_string <- rlang::as_label(rlang::enquo(group_column))
  replicate_column_string <- rlang::as_label(rlang::enquo(replicate_column))
  batch_column_string <- rlang::as_label(rlang::enquo(batch_column))

  collapse_helper(collapse_fn = max, collapse_fn_string = "max", data = data, feature_metadata_cols, sample_metadata_cols, separator, group_column = {{ group_column }}, replicate_column = {{ replicate_column }}, batch_column = {{ batch_column }}, group_column_string = group_column_string, replicate_column_string = replicate_column_string, batch_column_string = batch_column_string)
}
