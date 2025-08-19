test_that("NAs were correctly imputed", {
  min_int <- min(toy_metaboscape$Intensity, na.rm = T)

  nas <- which(is.na(toy_metaboscape$Intensity))

  toy_metaboscape_edit <- toy_metaboscape %>%
    impute_global_lowest()

  expect_true(all(toy_metaboscape_edit$Intensity[nas] == min_int))
})

test_that("Non-NAs stay unchanged", {
  non_nas <- which(!is.na(toy_metaboscape$Intensity))

  intensities_before <- toy_metaboscape$Intensity[non_nas]

  toy_metaboscape_edit <- toy_metaboscape %>%
    impute_global_lowest()

  intensities_after <- toy_metaboscape_edit$Intensity[non_nas]

  expect_equal(intensities_before, intensities_after)
})

test_that("row & column order stays unchanged", {
  imputed_df <- toy_metaboscape %>%
    impute_global_lowest()

  expect_equal(dplyr::select(imputed_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
