# wrapper function to use with purrr::quietly
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

