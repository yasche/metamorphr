#' Join a featuretable and sample metadata
#'
#' @description
#' Joins a featuretable and associated sample metadata. Basically a wrapper around \code{\link[dplyr]{left_join}} where `by = "Sample"`.
#'
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



featuretable_summary <- function(data) {

  column_names <- colnames(data)

  samples <- featuretable_summary_pull(data = data, select_what = "Sample")
  features <- featuretable_summary_pull(data = data, select_what = 2)

  n_samples <- samples %>%
    length()
  n_features <- features %>%
    length()

  n_samples_max <- 5L
  featuretable_summary_cat(txt = samples, title = "Samples", n = n_samples, n_max = n_samples_max)

  n_features_max <- 5L
  featuretable_summary_cat(txt = features, title = "Features", n = n_features, n_max = n_samples_max)

  data <- data %>%
    dplyr::group_by(.data$Sample) %>%
    tidyr::nest() %>%
    dplyr::mutate(summary = purrr::map(.data$data, function(x) {summary(x$Intensity)})) %>%
    dplyr::pull(summary)

  #is metadata present?
  if ("Group" %in% column_names) {
    groups <- data %>%
      dplyr::select("Group") %>%
      dplyr::distinct() %>%
      dplyr::pull()

    n_groups <- groups %>%
      length()

    n_groups_max <- 5L
    if (n_groups > n_groups_max) {
      features <- c(head(groups, n = n_groups_max))
    }

    #general information
    #cat(crayon::blue(as.character(n_samples), "Samples: "), paste(samples, collapse = ", "), "\n")
    #if (n_samples > n_samples_max) {
    #  cat(crayon::silver("#", as.character(n_samples - n_samples_max), "more samples\n"), sep = " ")
    #}
  }
}


featuretable_summary_pull <- function(data, select_what) {
  data %>%
    dplyr::select(select_what) %>%
    dplyr::distinct() %>%
    dplyr::pull()
}

featuretable_summary_cat <- function(txt, title, n, n_max) {
  if (n > n_max) {
    txt <- c(head(txt, n = n_max))
  }

  cat(crayon::blue(as.character(n), title, ": "), paste(txt, collapse = ", "), "\n")
  if (n > n_max) {
    cat(crayon::silver("#", as.character(n - n_max), "more",  tolower(title), "\n"), sep = " ")
  }
}
