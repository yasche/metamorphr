test_that("throws error if sample_metadata_cols is faulty", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata2) %>%
                 impute_lod() %>%
                 collapse_median(group_column = Group, replicate_column = Replicate, sample_metadata_cols = "Factor"))
})


test_that("throws error if feature_metadata_cols is faulty", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata2) %>%
                 impute_lod() %>%
                 collapse_median(group_column = Group, replicate_column = Replicate, feature_metadata_cols = c("Feature", "Factor")))
})

test_that("calculates the correct results and creates correct names for batch_column = NULL", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  calced_result <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata2) %>%
    impute_lod() %>%
    collapse_median(group_column = Group, replicate_column = Replicate) %>%
    dplyr::select(UID, Sample, Intensity) %>%
    dplyr::arrange(UID, Sample)

  expected_result <- test_collapse_median %>%
    dplyr::select(-Feature) %>%
    dplyr::arrange(UID, Sample)

  expect_equal(calced_result, expected_result)
})
