test_that("result is as expected for sample data", {
  calced_metadata <- test_scale_input %>%
    dplyr::select(Sample) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Group = stringr::str_sub(Sample, 1, 1))

  calced_result <- test_scale_input %>%
    join_metadata(calced_metadata) %>%
    scale_vast_grouped() %>%
    dplyr::select(-Group)

  expect_equal(test_scale_vast_s_results, calced_result)
})

test_that("row & column order stays unchanged", {
  joined_df <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata)

  scaled_df <- joined_df %>%
    impute_lod() %>%
    scale_vast_grouped()

  expect_equal(dplyr::select(scaled_df, -Intensity), dplyr::select(joined_df, -Intensity))
})
