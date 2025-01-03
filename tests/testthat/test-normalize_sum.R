test_that("Sum of intensites is 1 after normalization", {
  nearones <- toy_metaboscape %>%
    normalize_sum() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(Sum = sum(.data$Intensity, na.rm = TRUE)) %>%
    #use dplyr::near to account for rounding error
    dplyr::mutate(nearone = dplyr::near(.data$Sum, 1)) %>%
    dplyr::pull(nearone)

  expect_true(all(nearones))
})
