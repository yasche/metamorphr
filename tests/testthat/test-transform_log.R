test_that("transform_log(base = 10) calculates log10 correctly", {
  calced_pkg <- toy_metaboscape %>%
    transform_log(base = 10) %>%
    dplyr::pull(Intensity)

  calced_log10 <- toy_metaboscape %>%
    dplyr::pull(Intensity)

  calced_log10 <- log10(calced_log10)

  expect_equal(calced_pkg, calced_log10)
})
