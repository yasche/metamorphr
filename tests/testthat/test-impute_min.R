test_that("impute_min works", {
  toy_metaboscape_imputed <- toy_metaboscape %>%
    impute_min()

  expect_equal(toy_metaboscape_imputed, test_impute_min)
})
