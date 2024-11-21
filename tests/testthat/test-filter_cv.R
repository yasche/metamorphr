test_that("filter_cv() works for max_cv = 0", {
  filtered_features <- test_filters %>%
    filter_cv(reference_samples = c("q1", "q2", "q3"), max_cv = 0) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, "f0")
})


test_that("filter_cv() works for max_cv = 0.1", {
  filtered_features <- test_filters %>%
    filter_cv(reference_samples = c("q1", "q2", "q3"), max_cv = 0.1) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1"))
})


test_that("filter_cv() works for max_cv = 0.4", {
  filtered_features <- test_filters %>%
    filter_cv(reference_samples = c("q1", "q2", "q3"), max_cv = 0.4) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4"))
})


test_that("filter_cv() works for max_cv = 0.8", {
  filtered_features <- test_filters %>%
    filter_cv(reference_samples = c("q1", "q2", "q3"), max_cv = 0.8) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8"))
})


test_that("filter_cv() works for max_cv = 1.2", {
  filtered_features <- test_filters %>%
    filter_cv(reference_samples = c("q1", "q2", "q3"), max_cv = 1.2) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_grouped_mv() does not change the structure of the input in an unexpected way", {
  filtered_features <- test_filters %>%
    filter_cv(reference_samples = c("q1", "q2", "q3"), max_cv = Inf)

  expect_equal(filtered_features, test_filters)
})
