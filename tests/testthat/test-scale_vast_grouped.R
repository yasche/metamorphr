test_that("result is as expected for sample data", {
  calced_metadata <- test_scale_input %>%
    dplyr::select(Sample) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Group = stringr::str_sub(Sample, 1, 1))

  calced_result <- test_scale_input %>%
    join_metadata(calced_metadata) %>%
    scale_vast_grouped() %>%
    dplyr::select(-Group)

  expect_equal(test_scale_vast_s_results, calced_result)
})
