test_that("throws error if `data` is not tibble", {
  expect_error(convert_from_wide("abc"), "`data` must be of class `tibble` or `data.frame`, not `character`.")
  expect_error(convert_from_wide(matrix()), "`data` must be of class `tibble` or `data.frame`, not `matrix`. To convert a matrix use function `convert_from_matrix`.")
})

test_that("works for tibble/df", {
 converted <- readr::read_csv(system.file("extdata", "toy_metaboscape.csv", package = "metamorphr"), show_col_types = FALSE) %>%
   convert_from_wide(metadata_cols = 2:5)

 expect_equal(converted, toy_metaboscape)
})

test_that("always returns tibble", {
  tib <- readr::read_csv(system.file("extdata", "toy_metaboscape.csv", package = "metamorphr"), show_col_types = FALSE)
  df <- tib %>%
    as.data.frame()

  expect_false(tibble::is_tibble(df))

  tib <- convert_from_wide(tib, metadata_cols = 2:5)
  df <- convert_from_wide(df, metadata_cols = 2:5)

  expect_true(tibble::is_tibble(tib))
  expect_true(tibble::is_tibble(df))
})
