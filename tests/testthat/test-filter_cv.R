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


test_that("filter_cv() works for max_cv = 0, ref_as_group = TRUE", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1) %>%
    filter_cv(max_cv = 0, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, "f0")
})


test_that("filter_cv() works for max_cv = 0.1, ref_as_group = TRUE", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1) %>%
    filter_cv(max_cv = 0.1, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1"))
})


test_that("filter_cv() works for max_cv = 0.4, ref_as_group = TRUE", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1) %>%
    filter_cv(max_cv = 0.4, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4"))
})


test_that("filter_cv() works for max_cv = 0.8, ref_as_group = TRUE", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1) %>%
    filter_cv(max_cv = 0.8, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8"))
})


test_that("filter_cv() works for max_cv = 1.2, ref_as_group = TRUE", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1) %>%
    filter_cv(max_cv = 1.2, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_grouped_mv() does not change the structure of the input in an unexpected way, ref_as_group = TRUE", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1) %>%
    filter_cv(max_cv = Inf, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group)

  expect_equal(filtered_features, join_metadata(test_filters, test_filters_metadata1))
})

test_that("filter_cv() produces equal results for ref_as_group = TRUE and FALSE, 1", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1)

  filtered_features_groups <- filtered_features %>%
    filter_cv(max_cv = 0.4, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group)

  filtered_features_features <- filtered_features %>%
    filter_cv(max_cv = 0.4, reference_samples = c("q1", "q2", "q3"))

  expect_equal(filtered_features_groups, filtered_features_features)
})

test_that("filter_cv() produces equal results for ref_as_group = TRUE and FALSE, 2", {
  filtered_features <- test_filters %>%
    join_metadata(test_filters_metadata1)

  filtered_features_groups <- filtered_features %>%
    filter_cv(max_cv = 0.8, reference_samples = c("q"), ref_as_group = TRUE, group_column = Group)

  filtered_features_features <- filtered_features %>%
    filter_cv(max_cv = 0.8, reference_samples = c("q1", "q2", "q3"))

  expect_equal(filtered_features_groups, filtered_features_features)
})

test_that("filter_cv() does not throw a warning if group = .data$Group", {
  expect_no_warning(toy_metaboscape %>%
                      join_metadata(toy_metaboscape_metadata) %>%
                      filter_cv(reference_samples = "QC", ref_as_group = T, group_column = .data$Group))
})
