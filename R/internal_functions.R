# wrapper function to use with purrr::quietly/purrr::safely
# See https://search.r-project.org/CRAN/refmans/purrr/html/faq-adverbs-export.html
knn_impute_wrapper <- function(data_obs, ...) {
  withr::with_preserve_seed(impute::impute.knn(data_obs, ...))
}

knn_impute_quiet <- function(...) {
  purrr::quietly(knn_impute_wrapper)(...)
}

t_test_safely <- function(...) {
  purrr::safely(stats::t.test, otherwise = NA)(...)
}

glance_safely <- function(...) {
  purrr::safely(broom::glance, otherwise = NA)(...)$result
}

pull_safely <- function(...) {
  purrr::safely(dplyr::pull, otherwise = NA)(...)$result
}

# wrapper functions to use with testthat::local_mocked_bindings
# See https://testthat.r-lib.org/reference/local_mocked_bindings.html

is_installed_wrapper <- function(...) {
  rlang::is_installed(...)
}

check_installed_wrapper <- function(...) {
  rlang::check_installed(...)
}

internal_t_test <- function(data, group_column, groups_to_compare, ...) {
  group_1 <- data %>%
    dplyr::filter({{ group_column }} == groups_to_compare[[1]]) %>%
    dplyr::select("Intensity") %>%
    dplyr::pull()

  group_2 <- data %>%
    dplyr::filter({{ group_column }} == groups_to_compare[[2]]) %>%
    dplyr::select("Intensity") %>%
    dplyr::pull()

  t_test_safely(group_1, group_2, ...)$result
}

internal_l2fc <- function(data, group_column, groups_to_compare, log2_before) {
  if (log2_before == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = 2^.data$Intensity)
  }

  group_1 <- data %>%
    dplyr::filter({{ group_column }} == groups_to_compare[[1]]) %>%
    dplyr::select("Intensity") %>%
    dplyr::pull() %>%
    mean(na.rm = T) %>%
    log2()


  group_2 <- data %>%
    dplyr::filter({{ group_column }} == groups_to_compare[[2]]) %>%
    dplyr::select("Intensity") %>%
    dplyr::pull() %>%
    mean(na.rm = T) %>%
    log2()

  group_2 - group_1
}

internal_prep_pca_imputes <- function(data, direction) {
  # get column order for later
  data_colorder <- colnames(data)

  # get sample and feature metadata
  data_rownums <- 1:nrow(data)
  metadata <- data %>%
    dplyr::select(-"Intensity") %>%
    dplyr::mutate(RowNum = .env$data_rownums)

  # remove sample metadata
  data <- dplyr::select(data, "UID", "Sample", "Intensity")

  if (direction == 1) {
    data <- tidyr::spread(data, key = "Sample", value = "Intensity")
  } else if (direction == 2) {
    data <- tidyr::spread(data, key = "UID", value = "Intensity")
  } else {
    rlang::abort(paste0("`direction` must be 1 or 2, not ", as.character(direction), "."))
  }

  data_rownames <- data[[1]]

  data <- data %>%
    dplyr::select(-1) %>%
    as.matrix()

  rownames(data) <- data_rownames

  list(data = data,
       data_colorder = data_colorder,
       metadata = metadata)
}

internal_clean_pca_results <- function(data, direction, data_list) {

  data_colorder <- data_list$data_colorder
  metadata <- data_list$metadata

  if (direction == 1) {
    data <- tibble::as_tibble(data, rownames = "UID") %>%
      tidyr::gather(key = "Sample", value = "Intensity", -"UID")
  } else if (direction == 2) {
    data <- tibble::as_tibble(data, rownames = "Sample") %>%
      tidyr::gather(key = "UID", value = "Intensity", -"Sample")
  }

  data %>%
    dplyr::mutate(UID = as.integer(.data$UID)) %>%
    dplyr::left_join(metadata, by = dplyr::join_by(Sample, UID)) %>%
    dplyr::relocate(dplyr::all_of(data_colorder)) %>%
    dplyr::arrange(RowNum) %>%
    dplyr::select(-"RowNum")

}
