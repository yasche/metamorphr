test_that("impute_mean works", {
  toy_metaboscape_imputed <- toy_metaboscape %>%
    impute_mean()

  expect_equal(toy_metaboscape_imputed, test_impute_mean)
})

test_that("row & column order stays unchanged", {
  imputed_df <- toy_metaboscape %>%
    dplyr::mutate(Intensity = dplyr::case_when(UID == 1 & Sample == "Sample1" ~ NA,
                                               .default = 1)) %>%
    impute_mean()

  expect_equal(dplyr::select(imputed_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
