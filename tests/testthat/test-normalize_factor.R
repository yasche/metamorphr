test_that("results are as expected", {
  calced <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    normalize_factor()

  expect_equal(calced, test_normalize_factor_results)
})


test_that("row & column order stays unchanged", {
  joined_df <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata)

  normalized_df <- joined_df %>%
    normalize_factor()

  expect_equal(dplyr::select(joined_df, -Intensity), dplyr::select(normalized_df, -Intensity))
})
