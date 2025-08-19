test_that("normalize_median makes median = 1", {
  nearones <- toy_metaboscape %>%
    normalize_median() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(Median = median(.data$Intensity, na.rm = TRUE)) %>%
    # use dplyr::near to account for rounding error
    dplyr::mutate(nearone = dplyr::near(.data$Median, 1)) %>%
    dplyr::pull(nearone)

  expect_true(all(nearones))
})


test_that("row & column order stays unchanged", {
  normalized_df <- toy_metaboscape %>%
    normalize_median()

  expect_equal(dplyr::select(normalized_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
