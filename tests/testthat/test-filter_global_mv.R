test_that("filter_global_mv() does not change the structure of the input in an unexpected way", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = TRUE, max_missing = 1)

  expect_equal(filtered_features, toy_metaboscape)
})


test_that("filter_global_mv() filters the correct features for a 0.1 fraction cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = TRUE, max_missing = 0.1) %>%
    dplyr::select(Feature) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("276.13647 Da 27.28 s", "417.23236 Da 60.08 s"))
})


test_that("filter_global_mv() filters the correct features for a 0.5 fraction cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = TRUE, max_missing = 0.5) %>%
    dplyr::select(Feature) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("161.10519 Da 26.98 s",
                                    "276.13647 Da 27.28 s",
                                    "304.24023 Da 32.86 s",
                                    "417.23236 Da 60.08 s",
                                    "104.10753 Da 170.31 s",
                                    "105.04259 Da 199.80 s",
                                    "745.09111 Da 382.23 s",
                                    "427.02942 Da 424.84 s"))
})


test_that("filter_global_mv() filters the correct features for a 3 sample cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = FALSE, max_missing = 3) %>%
    dplyr::select(Feature) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("161.10519 Da 26.98 s",
                                    "276.13647 Da 27.28 s",
                                    "304.24023 Da 32.86 s",
                                    "417.23236 Da 60.08 s",
                                    "104.10753 Da 170.31 s",
                                    "105.04259 Da 199.80 s",
                                    "745.09111 Da 382.23 s",
                                    "427.02942 Da 424.84 s"))
})

test_that("filter_global_mv() filters the correct features for a 1 sample cutoff", {
  filtered_features <- toy_metaboscape %>%
    filter_global_mv(fraction = FALSE, max_missing = 1) %>%
    dplyr::select(Feature) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("276.13647 Da 27.28 s", "417.23236 Da 60.08 s"))
})
