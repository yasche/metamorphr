test_that("impute_median works", {
  toy_metaboscape_imputed <- toy_metaboscape %>%
    impute_median()

  expect_equal(toy_metaboscape_imputed, test_impute_median)
})

test_that("row & column order stays unchanged", {
  imputed_df <- toy_metaboscape %>%
    dplyr::mutate(Intensity = dplyr::case_when(UID == 1 & Sample == "Sample1" ~ NA,
                                               .default = 1)) %>%
    impute_median()

  expect_equal(dplyr::select(imputed_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
