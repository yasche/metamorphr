test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_range()

  expect_equal(test_scale_range_results, calced_result)
})
