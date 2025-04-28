test_that("impute_median works", {
  toy_metaboscape_imputed <- toy_metaboscape %>%
    impute_median()

  expect_equal(toy_metaboscape_imputed, test_impute_median)
})

