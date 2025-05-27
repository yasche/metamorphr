test_that("throws error if !(direction %in% c(1,2))", {
  expect_error(internal_prep_pca_imputes(toy_metaboscape, direction = 3))
})
