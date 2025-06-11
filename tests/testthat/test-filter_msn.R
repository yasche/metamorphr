test_that("returns empty tibble if no fragments are found", {
  mgf_tibble <- read_mgf(test_path("data", "test_read_mgf.mgf"))

  filtered <- filter_msn(mgf_tibble, fragments = c(1000, 2000), tolerance = 5, tolerance_type = "ppm", min_found = 2)

  expect_equal(nrow(filtered), 0)
  expect_equal(colnames(filtered), c("VARIABLEONE", "VARIABLETWO", "VARIABLETHREE", "MSn"))
})


test_that("filters correctly for tolerance_type = 'ppm'", {
  mgf_tibble <- read_mgf(test_path("data", "test_read_mgf.mgf"))

  filtered_1 <- filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 34.567), tolerance = 0, tolerance_type = "ppm", min_found = 3) # expect row 1
  filtered_2 <- filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 1000), tolerance = 0, tolerance_type = "ppm", min_found = 3) # expect empty tibble
  filtered_3 <- filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 1000), tolerance = 0, tolerance_type = "ppm", min_found = 2) # expect row 1

  filtered_4 <- filter_msn(mgf_tibble, fragments = c(123.455, 234.565, 345.675), tolerance = 10, tolerance_type = "ppm", min_found = 3) # expect row 2
  filtered_5 <- filter_msn(mgf_tibble, fragments = c(123.455, 234.565, 345.675), tolerance = 5, tolerance_type = "ppm", min_found = 3) # expect empty tibble
  filtered_6 <- filter_msn(mgf_tibble, fragments = c(123.455, 234.565, 1000), tolerance = 10, tolerance_type = "ppm", min_found = 2) # expect row 2
  filtered_7 <- filter_msn(mgf_tibble, fragments = c(123.455, 2000, 1000), tolerance = 10, tolerance_type = "ppm", min_found = 1) # expect row 2

  expect_equal(filtered_1[[1]][[1]], 1.1)
  expect_equal(nrow(filtered_2), 0)
  expect_equal(filtered_3[[1]][[1]], 1.1)
  expect_equal(filtered_4[[1]][[1]], 2.1)
  expect_equal(nrow(filtered_5), 0)
  expect_equal(filtered_6[[1]][[1]], 2.1)
  expect_equal(filtered_7[[1]][[1]], 2.1)
})

test_that("filters correctly for tolerance_type = 'absolute'", {
  mgf_tibble <- read_mgf(test_path("data", "test_read_mgf.mgf"))

  filtered_1 <- filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 34.567), tolerance = 0, tolerance_type = "absolute", min_found = 3) # expect row 1
  filtered_2 <- filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 1000), tolerance = 0, tolerance_type = "absolute", min_found = 3) # expect empty tibble
  filtered_3 <- filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 1000), tolerance = 0, tolerance_type = "absolute", min_found = 2) # expect row 1

  filtered_4 <- filter_msn(mgf_tibble, fragments = c(123.455, 234.565, 345.675), tolerance = 0.003, tolerance_type = "absolute", min_found = 3) # expect row 2
  filtered_5 <- filter_msn(mgf_tibble, fragments = c(123.455, 234.565, 345.675), tolerance = 0.001, tolerance_type = "absolute", min_found = 3) # expect empty tibble
  filtered_6 <- filter_msn(mgf_tibble, fragments = c(123.455, 234.565, 1000), tolerance = 0.003, tolerance_type = "absolute", min_found = 2) # expect row 2
  filtered_7 <- filter_msn(mgf_tibble, fragments = c(123.455, 2000, 1000), tolerance = 0.003, tolerance_type = "absolute", min_found = 1) # expect row 2

  expect_equal(filtered_1[[1]][[1]], 1.1)
  expect_equal(nrow(filtered_2), 0)
  expect_equal(filtered_3[[1]][[1]], 1.1)
  expect_equal(filtered_4[[1]][[1]], 2.1)
  expect_equal(nrow(filtered_5), 0)
  expect_equal(filtered_6[[1]][[1]], 2.1)
  expect_equal(filtered_7[[1]][[1]], 2.1)
})

test_that("throws error if tolerance_type != c('absolute', 'ppm')", {
  mgf_tibble <- read_mgf(test_path("data", "test_read_mgf.mgf"))

  expect_error(filter_msn(mgf_tibble, fragments = c(12.345, 23.456, 34.567), tolerance = 0, tolerance_type = "absolutexxx", min_found = 3), 'Argument `tolerance_type` must be "ppm" or "absolute", not "absolutexxx".')
})
