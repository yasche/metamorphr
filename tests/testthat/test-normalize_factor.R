test_that("results are as expected", {
  calced <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    normalize_factor()

  expect_equal(calced, test_normalize_factor_results)
})
