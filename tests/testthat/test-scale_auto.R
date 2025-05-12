test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_auto()

  expect_equal(test_scale_auto_results, calced_result)
})
