test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_range()

  expect_equal(test_scale_range_results, calced_result)
})

test_that("row & column order stays unchanged", {
  scaled_df <- toy_metaboscape %>%
    impute_lod() %>%
    scale_range()

  expect_equal(dplyr::select(scaled_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
