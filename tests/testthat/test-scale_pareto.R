test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_pareto()

  expect_equal(test_scale_pareto_results, calced_result)
})
