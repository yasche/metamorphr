test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_level()

  expect_equal(test_scale_level_results, calced_result)
})
