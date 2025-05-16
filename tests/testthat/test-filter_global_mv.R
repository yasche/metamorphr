test_that("filter_global_mv() does not change the structure of the input in an unexpected way", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = TRUE, min_found = 0)

  expect_equal(filtered_features, toy_metaboscape)
})


test_that("filter_global_mv() filters the correct features for a 0.1 fraction cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = TRUE, min_found = 0.9) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("276.13647 Da 27.28 s", "417.23236 Da 60.08 s"))
})


test_that("filter_global_mv() filters the correct features for a 0.5 fraction cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = TRUE, min_found = 0.5) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c(
    "161.10519 Da 26.98 s",
    "276.13647 Da 27.28 s",
    "304.24023 Da 32.86 s",
    "417.23236 Da 60.08 s",
    "104.10753 Da 170.31 s",
    "105.04259 Da 199.80 s",
    "745.09111 Da 382.23 s",
    "427.02942 Da 424.84 s"
  ))
})


test_that("filter_global_mv() filters the correct features for a 8 sample cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = FALSE, min_found = 8) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c(
    "161.10519 Da 26.98 s",
    "276.13647 Da 27.28 s",
    "304.24023 Da 32.86 s",
    "417.23236 Da 60.08 s",
    "104.10753 Da 170.31 s",
    "105.04259 Da 199.80 s",
    "745.09111 Da 382.23 s",
    "427.02942 Da 424.84 s"
  ))
})

test_that("filter_global_mv() filters the correct features for a 10 sample cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = FALSE, min_found = 10) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("276.13647 Da 27.28 s", "417.23236 Da 60.08 s"))
})

# new tests with a better data set
# if blanks and qc are filterd out, then f0 is found in 0 samples, f1 in 1, f2 in 2, f3 in 3...
test_that("filter_global_mv() filters the correct features for a 0 sample cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = FALSE, min_found = 0) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_global_mv() filters the correct features for a 1 sample cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = FALSE, min_found = 1) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_global_mv() filters the correct features for a 5 sample cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = FALSE, min_found = 5) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_global_mv() filters the correct features for a 8 sample cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = FALSE, min_found = 8) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_global_mv() filters the correct features for a 12 sample cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = FALSE, min_found = 12) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f12"))
})


test_that("filter_global_mv() filters the correct features for a 0/12 cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = TRUE, min_found = 0 / 12) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_global_mv() filters the correct features for a 1/12 sample cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = TRUE, min_found = 1 / 12) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_global_mv() filters the correct features for a 5/12 cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = TRUE, min_found = 5 / 12) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_global_mv() filters the correct features for a 8/12 cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = TRUE, min_found = 8 / 12) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_global_mv() filters the correct features for a 12/12 cutoff", {
  filtered_features <- test_filters %>%
    # only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_global_mv(fraction = TRUE, min_found = 12 / 12) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f12"))
})

test_that("filter_global_mv() throws error if min_found > 1 and fraction = T", {
  expect_error(toy_metaboscape %>%
    filter_global_mv(min_found = 2, fraction = T))
})
