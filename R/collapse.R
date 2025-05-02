collapse_helper <- function(collapse_fn, collapse_fn_string, data, feature_metadata_cols, sample_metadata_cols, separator, group_column_string, replicate_column_string, batch_column_string) {

  if (batch_column_string == "NULL") {

    #define the cols to keep in returned data frame.
    #by default, those are the feature meta data UID, Feature, Intensity, group_column. replicate_column and the columns defined in feature_metadata_cols
    #those are needed to restore column order later
    cols_to_keep <- c("UID",
                      "Intensity",
                      "Sample",
                      group_column_string,
                      replicate_column_string,
                      feature_metadata_cols)

    if (!is.null(sample_metadata_cols)) {
      cols_to_keep <- c(cols_to_keep, sample_metadata_cols)
    }

    cols_to_keep <- colnames(data)[colnames(data) %in% cols_to_keep]

    #feature metadata to join back with the original df later
    feature_metadata <- dplyr::select(data, "UID", dplyr::all_of(feature_metadata_cols)) %>%
      dplyr::distinct()

    #sample metadata to join back with the original df later
    sample_metadata <- dplyr::select(data, dplyr::all_of(c(group_column_string, replicate_column_string)), dplyr::all_of(sample_metadata_cols)) %>%
      dplyr::distinct()

    #print(sample_metadata)

    data <- data %>%
      dplyr::group_by(.data$UID, dplyr::across(dplyr::all_of(c(group_column_string, replicate_column_string)))) %>%
      dplyr::summarise(Intensity = collapse_fn(.data$Intensity, na.rm = T)) %>%
      dplyr::ungroup()

    data$Sample <- paste0(dplyr::pull(data, group_column_string),
                          separator,
                          dplyr::pull(data, replicate_column_string))

    #sanity check: are feature and sample metadata correctly assigned?
    length_sample_metadata <- nrow(sample_metadata)
    length_sample_metadata_expected <- data %>%
      dplyr::select("Sample") %>%
      dplyr::distinct() %>%
      nrow()

    if (!length_sample_metadata == length_sample_metadata_expected) {
      print(sample_metadata, n = nrow(sample_metadata))
      stop(paste0("\nThere is a problem in the 'sample_metadata_cols' argument you have provided.
Some observations in the columns you specified in 'sample_metadata_cols' (", paste(sample_metadata_cols, collapse = ", "), ") are not unique for a given ", group_column_string, " ", replicate_column_string, " combination.
Each combination of ", group_column_string, " and ", replicate_column_string, " must only appear once in the table above.\n
Did you provide feature metadata as sample metadata? See ?collapse_", collapse_fn_string, "for more information."))
    }

    length_feature_metadata <- nrow(feature_metadata)
    length_feature_metadata_expected <- data %>%
      dplyr::select("UID") %>%
      dplyr::distinct() %>%
      nrow()

    if (!length_feature_metadata == length_feature_metadata_expected) {
      print(feature_metadata, n = nrow(feature_metadata))
      stop(paste0("\nThere is a problem in the 'feature_metadata_cols' argument you have provided.
Some observations in the columns you specified in 'feature_metadata_cols' (", paste(feature_metadata_cols, collapse = ", "), ") are not unique for a given UID.
Each UID must only appear once in the table above.\n
Did you provide sample metadata as feature metadata? See ?collapse_", collapse_fn_string, " for more information."))
    }

    data <- data %>%
      dplyr::left_join(feature_metadata, by = "UID") %>%
      dplyr::left_join(sample_metadata, by = c(group_column_string, replicate_column_string)) %>%
      dplyr::relocate(dplyr::all_of(cols_to_keep))

    data

  } else {

    #define the cols to keep in returned data frame.
    #by default, those are the feature meta data UID, Feature, Intensity, group_column. replicate_column and the columns defined in feature_metadata_cols
    #those are needed to restore column order later
    cols_to_keep <- c("UID",
                      "Intensity",
                      "Sample",
                      group_column_string,
                      replicate_column_string,
                      batch_column_string,
                      feature_metadata_cols)

    if (!is.null(sample_metadata_cols)) {
      cols_to_keep <- c(cols_to_keep, sample_metadata_cols)
    }

    cols_to_keep <- colnames(data)[colnames(data) %in% cols_to_keep]

    #feature metadata to join back with the original df later
    feature_metadata <- dplyr::select(data, "UID", dplyr::all_of(feature_metadata_cols)) %>%
      dplyr::distinct()


    #sample metadata to join back with the original df later
    sample_metadata <- dplyr::select(data, dplyr::all_of(c(group_column_string, replicate_column_string, batch_column_string)), dplyr::all_of(sample_metadata_cols)) %>%
      dplyr::distinct()


    data <- data %>%
      dplyr::group_by(.data$UID, dplyr::across(dplyr::all_of(c(group_column_string, replicate_column_string, batch_column_string)))) %>%
      dplyr::summarise(Intensity = collapse_fn(.data$Intensity, na.rm = T)) %>%
      dplyr::ungroup()

    data$Sample <- paste0(dplyr::pull(data, group_column_string),
                          separator,
                          dplyr::pull(data, batch_column_string),
                          separator,
                          dplyr::pull(data, replicate_column_string))

    #sanity check: are feature and sample metadata correctly assigned?
    length_sample_metadata <- nrow(sample_metadata)
    length_sample_metadata_expected <- data %>%
      dplyr::select("Sample") %>%
      dplyr::distinct() %>%
      nrow()

    if (!length_sample_metadata == length_sample_metadata_expected) {
      print(sample_metadata, n = nrow(sample_metadata))
      stop(paste0("\nThere is a problem in the 'sample_metadata_cols' argument you have provided.
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
      stop(paste0("\nThere is a problem in the 'feature_metadata_cols' argument you have provided.
Some observations in the columns you specified in 'feature_metadata_cols' (", paste(feature_metadata_cols, collapse = ", "), ") are not unique for a given UID.
Each UID must only appear once in the table above.\n
Did you provide sample metadata as feature metadata? See ?collapse_", collapse_fn_string, " for more information."))
    }

    data <- data %>%
      dplyr::left_join(feature_metadata, by = "UID") %>%
      dplyr::left_join(sample_metadata, by = c(group_column_string, replicate_column_string, batch_column_string)) %>%
      dplyr::relocate(dplyr::all_of(cols_to_keep))

    data
  }
}

#' Collapse technical replicates by calculating their mean
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
#' @param batch_column If there are different batches in the tibble, that should be considered you can specify them with the `batch_column` argument. If batch information should not be used, set `batch_column = NULL`. Otherwise, usually `batch_column = Batch`.
#' @param feature_metadata_cols A character or character vector containing the names of the feature metadata columns. They are usually created when reading the feature table with \code{\link[metamorphr]{read_featuretable}}. Feature metadata columns not specified here will be dropped.
#' @param sample_metadata_cols A character or character vector containing the names of the sample metadata columns. They are usually created when joining the metadata with \code{\link[metamorphr]{join_metadata}}. Sample metadata columns not specified here will be dropped, except for `group_column`, `replicate_column` and `batch_column` if specified.
#' @param separator Separator used for joining group and replicate, or group, batch and replicate together to create the new sample names.
#'
#' @return A tibble with intensities of technical replicates collapsed.
#' @export
#'
#' @examples
#' #uses a slightly modified version of toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata$Replicate <- 1
#'
#' toy_metaboscape %>%
#'   join_metadata(collapse_toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   collapse_mean(group_column = Group, replicate_column = Replicate)
#'
#'
collapse_mean <- function(data, group_column, replicate_column, batch_column = NULL, feature_metadata_cols = "Feature", sample_metadata_cols = NULL, separator = "_") {
  group_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(group_column))))
  replicate_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(replicate_column))))
  batch_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(batch_column))))

  collapse_helper(collapse_fn = mean, collapse_fn_string = "mean", data =  data, feature_metadata_cols, sample_metadata_cols, separator, group_column_string, replicate_column_string, batch_column_string)
}


#' Collapse technical replicates by calculating their median
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
#' @param batch_column If there are different batches in the tibble, that should be considered you can specify them with the `batch_column` argument. If batch information should not be used, set `batch_column = NULL`. Otherwise, usually `batch_column = Batch`.
#' @param feature_metadata_cols A character or character vector containing the names of the feature metadata columns. They are usually created when reading the feature table with \code{\link[metamorphr]{read_featuretable}}. Feature metadata columns not specified here will be dropped.
#' @param sample_metadata_cols A character or character vector containing the names of the sample metadata columns. They are usually created when joining the metadata with \code{\link[metamorphr]{join_metadata}}. Sample metadata columns not specified here will be dropped, except for `group_column`, `replicate_column` and `batch_column` if specified.
#' @param separator Separator used for joining group and replicate, or group, batch and replicate together to create the new sample names.
#'
#' @return A tibble with intensities of technical replicates collapsed.
#' @export
#'
#' @examples
#' #uses a slightly modified version of toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata <- toy_metaboscape_metadata
#' collapse_toy_metaboscape_metadata$Replicate <- 1
#'
#' toy_metaboscape %>%
#'   join_metadata(collapse_toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   collapse_median(group_column = Group, replicate_column = Replicate)
#'
#'
collapse_median <- function(data, group_column, replicate_column, batch_column = NULL, feature_metadata_cols = "Feature", sample_metadata_cols = NULL, separator = "_") {
  group_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(group_column))))
  replicate_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(replicate_column))))
  batch_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(batch_column))))

  collapse_helper(collapse_fn = stats::median, collapse_fn_string = "median", data =  data, feature_metadata_cols, sample_metadata_cols, separator, group_column_string, replicate_column_string, batch_column_string)
}
