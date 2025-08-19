test_that("transform_log(base = 10) calculates log10 correctly", {
  calced_pkg <- toy_metaboscape %>%
    transform_log(base = 10) %>%
    dplyr::pull(Intensity)

  calced_log10 <- toy_metaboscape %>%
    dplyr::pull(Intensity)

  calced_log10 <- log10(calced_log10)

  expect_equal(calced_pkg, calced_log10)
})

test_that("row & column order stays unchanged", {
  transformed_df <- toy_metaboscape %>%
    impute_lod() %>%
    transform_log(base = 10)

  expect_equal(dplyr::select(transformed_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
