test_that("transform_power(n = 2) calculates sqrt correctly", {
  calced_pkg <- toy_metaboscape %>%
    transform_power(n = 2) %>%
    dplyr::pull(Intensity)

  calced_sqrt <- toy_metaboscape %>%
    dplyr::pull(Intensity)

  calced_sqrt <- sqrt(calced_sqrt)

  expect_equal(calced_pkg, calced_sqrt)
})

test_that("row & column order stays unchanged", {
  transformed_df <- toy_metaboscape %>%
    impute_lod() %>%
    transform_power()

  expect_equal(dplyr::select(transformed_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
