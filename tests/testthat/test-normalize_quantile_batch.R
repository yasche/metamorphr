test_that("normalize_quantile_batch produces expected results, when accounting for rounding error", {
  #Values in the publicationused for this evaluation are rounded to the 2nd digit (Y. Zhao, L. Wong, W. W. B. Goh, Sci Rep 2020, 10, 15534, DOI 10.1038/s41598-020-72664-6)
  #R may round differently, see ?round
  #therefore, differences between expected results and calculated results are first calculated.
  #As numbers are rounded to the 2nd digit, absolute differences should be less than or equal to 0.005.
  max_diff <- test_qn_data %>%
    join_metadata(test_qn_metadata) %>%
    normalize_quantile_batch(group_column = .data$Group, batch_column = .data$Batch) %>%
    dplyr::select(-"Feature") %>%
    dplyr::left_join(test_qn_batch_results, by = c("UID", "Sample")) %>%
    dplyr::mutate(Intenity_diff = .data$Intensity.x - .data$Intensity.y) %>%
    dplyr::pull(.data$Intenity_diff) %>%
    abs() %>%
    max()

  expect_lte(max_diff, 0.005)
})
