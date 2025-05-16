test_that("result is as expected for sample data", {
  calced_result <- test_scale_input %>%
    scale_vast()

  expect_equal(test_scale_vast_u_results, calced_result)
})
