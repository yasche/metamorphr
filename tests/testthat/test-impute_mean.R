test_that("impute_mean works", {
  toy_metaboscape_imputed <- toy_metaboscape %>%
    impute_mean()

  expect_equal(toy_metaboscape_imputed, test_impute_mean)
})
