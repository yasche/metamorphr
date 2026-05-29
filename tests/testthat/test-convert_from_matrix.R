test_that("throws error if `data` is not matrix", {
  expect_error(convert_from_matrix("abc"), "`data` must be of class `matrix`, not `character`.")
  expect_error(convert_from_matrix(tibble::tibble()), "`data` must be of class `matrix`, not `data.frame`. To convert a data frame use function `convert_from_wide`.")
})

test_that("works for matrix with toy data set", {
  dataset <- matrix(1:9, ncol = 3)
  colnames(dataset) <- paste0("sample", 1:3)
  rownames(dataset) <- paste0("feature", 1:3)

  dataset_t <- t(dataset)

  expect_true(is.matrix(dataset))
  expect_equal(convert_from_matrix(dataset), test_read_convert_from_matrix)
  expect_equal(convert_from_matrix(dataset_t, samples_in_cols = FALSE), test_read_convert_from_matrix)
  expect_equal(convert_from_matrix(dataset_t, samples_in_cols = FALSE), convert_from_matrix(dataset))
})

