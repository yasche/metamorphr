test_that("filter_grouped_mv() does not change the structure of the input in an unexpected way", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 0)

  expect_equal(filtered_features, feature_table_and_metadata)
})


test_that("filter_grouped_mv() filters the correct features for a 1 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 1) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 2 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 2) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 3 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 3) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_grouped_mv() filters the correct features for a 6 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 6) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 1/6 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 1/6) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 2/6 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 2/6) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 3/6 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 3/6) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 6/6 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata1, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 6/6) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 1 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 1) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 2 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 2) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 3 sample cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 3) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

test_that("filter_grouped_mv() filters the correct features for a 4 sample cutoff, 2 groups (empty vector)", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = FALSE, min_found = 4) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, character())
})

test_that("filter_grouped_mv() filters the correct features for a 1/3 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 1/3) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 2/3 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 2/3) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})


test_that("filter_grouped_mv() filters the correct features for a 3/3 cutoff, 2 groups", {

  feature_table_and_metadata <- test_filters %>%
    dplyr::left_join(test_filters_metadata2, by = "Sample")

  filtered_features <- feature_table_and_metadata %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3", "b1"))) %>%
    filter_grouped_mv(grouping_column = Group, fraction = TRUE, min_found = 3/3) %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"))
})

