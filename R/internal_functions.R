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
