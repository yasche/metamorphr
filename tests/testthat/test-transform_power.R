test_that("transform_power(n = 2) calculates sqrt correctly", {
  calced_pkg <- toy_metaboscape %>%
    transform_power(n = 2) %>%
    dplyr::pull(Intensity)

  calced_sqrt <- toy_metaboscape %>%
    dplyr::pull(Intensity)

  calced_sqrt <- sqrt(calced_sqrt)

  expect_equal(calced_pkg, calced_sqrt)
})
