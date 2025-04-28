test_that("NAs were correctly imputed, value = 1", {
  value <- 1

  nas <- which(is.na(toy_metaboscape$Intensity))

  toy_metaboscape_edit <- toy_metaboscape %>%
    impute_user_value(value = value)

  expect_true(all(toy_metaboscape_edit$Intensity[nas] == value))
})

test_that("NAs were correctly imputed, value = 5", {
  value <- 5

  nas <- which(is.na(toy_metaboscape$Intensity))

  toy_metaboscape_edit <- toy_metaboscape %>%
    impute_user_value(value = value)

  expect_true(all(toy_metaboscape_edit$Intensity[nas] == value))
})

test_that("Non-NAs stay unchanged", {
  value <- 1

  non_nas <- which(!is.na(toy_metaboscape$Intensity))

  intensities_before <- toy_metaboscape$Intensity[non_nas]

  toy_metaboscape_edit <- toy_metaboscape %>%
    impute_user_value(value = value)

  intensities_after <- toy_metaboscape_edit$Intensity[non_nas]

  expect_equal(intensities_before, intensities_after)
})
