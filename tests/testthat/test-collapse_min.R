test_that("throws error if sample_metadata_cols is faulty", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata2) %>%
                 impute_lod() %>%
                 collapse_min(group_column = Group, replicate_column = Replicate, sample_metadata_cols = "Factor"))
})


test_that("throws error if feature_metadata_cols is faulty", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata2) %>%
                 impute_lod() %>%
                 collapse_min(group_column = Group, replicate_column = Replicate, feature_metadata_cols = c("Feature", "Factor")))
})

test_that("calculates the correct results and creates correct names for batch_column = NULL", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  calced_result <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata2) %>%
    impute_lod() %>%
    collapse_min(group_column = Group, replicate_column = Replicate) %>%
    dplyr::select(UID, Sample, Intensity) %>%
    dplyr::arrange(UID, Sample)

  expected_result <- test_collapse_min %>%
    dplyr::select(-Feature) %>%
    dplyr::arrange(UID, Sample)

  expect_equal(calced_result, expected_result)
})


test_that("calculates the correct results and creates correct names for batch_column = Batch", {
  toy_metaboscape_metadata2 <- test_collapse_mean_batches %>%
    create_metadata_skeleton() %>%
    dplyr::mutate(Group = c(rep("blank", 2),
                            rep("QC", 3),
                            rep("control", 3),
                            rep("treatment", 3),
                            rep("blank", 2),
                            rep("control", 3),
                            rep("treatment", 3),
                            rep("blank", 2),
                            rep("control", 3),
                            rep("treatment", 3))) %>%
    dplyr::mutate(Replicate = 1) %>%
    dplyr::mutate(Batch = c(rep(1, 11),
                            rep(2, 8),
                            rep(3, 8)))


  calced_result <- test_collapse_mean_batches %>%
    join_metadata(toy_metaboscape_metadata2) %>%
    impute_lod() %>%
    collapse_min(group_column = Group, replicate_column = Replicate, batch_column = Batch) %>%
    dplyr::select(UID, Sample, Intensity) %>%
    dplyr::arrange(UID, Sample)

  expected_result <- test_collapse_min_batches_results %>%
    dplyr::select(-Feature) %>%
    dplyr::arrange(UID, Sample)

  expect_equal(calced_result, expected_result)
})

test_that("throws error if sample_metadata_cols is faulty, batch_column = Batch", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata2) %>%
                 impute_lod() %>%
                 collapse_min(group_column = Group, replicate_column = Replicate, sample_metadata_cols = "Factor", batch_column = Batch))
})


test_that("throws error if feature_metadata_cols is faulty, batch_column = Batch", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata2) %>%
                 impute_lod() %>%
                 collapse_min(group_column = Group, replicate_column = Replicate, feature_metadata_cols = c("Feature", "Factor"), batch_column = Batch))
})


test_that("calculates the correct results if standard arguments are used", {
  toy_metaboscape_metadata2 <- toy_metaboscape_metadata
  toy_metaboscape_metadata2$Replicate <- rep(1, nrow(toy_metaboscape_metadata2))

  calced_result <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata2) %>%
    impute_lod() %>%
    collapse_min() %>%
    dplyr::select(UID, Sample, Intensity) %>%
    dplyr::arrange(UID, Sample)

  expected_result <- test_collapse_min %>%
    dplyr::select(-Feature) %>%
    dplyr::arrange(UID, Sample)

  expect_equal(calced_result, expected_result)
})
