#' Join a featuretable and sample metadata
#'
#' @description
#' Joins a featuretable and associated sample metadata. Basically a wrapper around \code{\link[dplyr]{left_join}} where `by = "Sample"`.
#'
#' @param data A feature table created with \code{\link[metamorphr]{read_featuretable}}
#' @param metadata Sample metadata created with \code{\link[metamorphr]{create_metadata_skeleton}}
#'
#' @return A tibble with added sample metadata.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata)
join_metadata <- function(data, metadata) {
  dplyr::left_join(data, metadata, by = "Sample")
}



#' General information about a feature table and sample-wise summary
#'
#' @description
#' Information about a feature table. Prints information to the console (number of samples, number of features and if applicable number of groups,
#' replicates and batches) and returns a sample-wise summary as a list.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_samples_max How many Samples should be printed to the console?
#' @param n_features_max How many Features should be printed to the console?
#' @param n_groups_max How many groups should be printed to the console?
#' @param n_batches_max How many Batches should be printed to the console?
#'
#' @return A sample-wise summary as a list.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   summary_featuretable()
summary_featuretable <- function(data, n_samples_max = 5, n_features_max = 5, n_groups_max = 5, n_batches_max = 5) {
  column_names <- colnames(data)

  samples <- summary_featuretable_pull(data = data, select_what = "Sample")
  features <- summary_featuretable_pull(data = data, select_what = 2)

  n_samples <- length(samples)
  n_features <- length(features)

  summary_featuretable_cat(txt = samples, title = "Samples", n = n_samples, n_max = n_samples_max)
  summary_featuretable_cat(txt = features, title = "Features", n = n_features, n_max = n_samples_max)

  # is metadata present?
  if ("Group" %in% column_names) {
    groups <- summary_featuretable_pull(data = data, select_what = "Group")

    n_groups <- groups %>%
      length()

    summary_featuretable_cat(txt = groups, title = "Groups", n = n_groups, n_max = n_groups_max)
  }

  if ("Replicate" %in% column_names) {
    replicates <- summary_featuretable_pull(data = data, select_what = "Replicate")

    n_replicates <- length(replicates)

    if (n_replicates > 1) {
      cat(crayon::blue("Replicates detected: ", min(replicates), "...", max(replicates), "\n", sep = ""))
    }
  }

  if ("Batch" %in% column_names) {
    batches <- summary_featuretable_pull(data = data, select_what = "Batch")

    n_batches <- length(batches)

    summary_featuretable_cat(txt = batches, title = "Batches", n = n_batches, n_max = n_batches_max)
  }

  n_nas <- data %>%
    dplyr::filter(is.na(.data$Intensity)) %>%
    nrow()

  n_total <- nrow(data)

  cat(crayon::green(as.character(round((n_nas / n_total) * 100)), " % missing values (NA): ", sep = ""), paste0(as.character(n_nas), " out of ", as.character(n_total), "."), "\n", sep = "")

  data <- data %>%
    dplyr::group_by(.data$Sample) %>%
    tidyr::nest() %>%
    dplyr::mutate(summary = purrr::map(.data$data, function(x) {
      summary(x$Intensity)
    })) %>%
    dplyr::pull(summary)

  names(data) <- samples

  data
}


summary_featuretable_pull <- function(data, select_what) {
  data %>%
    dplyr::select(dplyr::all_of(select_what)) %>%
    dplyr::distinct() %>%
    dplyr::pull()
}


summary_featuretable_cat <- function(txt, title, n, n_max) {
  if (n > n_max) {
    txt <- c(utils::head(txt, n = n_max))
  }

  cat(crayon::blue(as.character(n), " ", title, ": ", sep = ""), paste(txt, collapse = ", "), "\n", sep = "")
  if (n > n_max) {
    cat(crayon::silver("# ", as.character(n - n_max), " more ", tolower(title), "\n", sep = ""))
    cat(crayon::silver("# ", "Use the n_", tolower(title), "_max", " argument to see more", "\n", sep = ""))
  }
}
