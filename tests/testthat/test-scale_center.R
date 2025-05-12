test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_center()

  expect_equal(test_scale_center_results, calced_result)
})
