test_that("returns empty tibble if no fragments are found", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble_nl <- calc_neutral_loss(mgf_tibble, m_z_col = PEPMASS)

  filtered <- filter_neutral_loss(mgf_tibble_nl, losses = c(1000, 2000), tolerance = 5, tolerance_type = "ppm", min_found = 2)

  expect_equal(nrow(filtered), 0)
  expect_equal(colnames(filtered), c("VARIABLEONE", "VARIABLETWO", "VARIABLETHREE", "PEPMASS", "MSn", "Neutral_Loss"))
})

test_that("stays unchanged for min_found = 0", {
  tbl_before <- read_mgf(test_path("data", "test_filter_msn_nl.mgf")) %>%
    calc_neutral_loss(m_z_col = PEPMASS)

  tbl_after <- filter_neutral_loss(tbl_before, losses = c(1000, 2000), tolerance = 5, tolerance_type = "ppm", min_found = 0)

  expect_equal(tbl_before, tbl_after)
})

test_that("filters correctly for tolerance_type = 'ppm'", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble <- calc_neutral_loss(mgf_tibble, m_z_col = PEPMASS)

  filtered_1 <- filter_neutral_loss(mgf_tibble, losses = c(11.1111, 22.2222, 33.3333), tolerance = 0.001, tolerance_type = "ppm", min_found = 3) # expect row 1; tolerance should not be 0 due to machine precission
  filtered_2 <- filter_neutral_loss(mgf_tibble, losses = c(11.1111, 22.2222, 1000), tolerance = 0.001, tolerance_type = "ppm", min_found = 3) # expect empty tibble
  filtered_3 <- filter_neutral_loss(mgf_tibble, losses = c(11.1111, 22.2222, 1000), tolerance = 0.001, tolerance_type = "ppm", min_found = 2) # expect row 1

  filtered_4 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 222.1122, 333.220), tolerance = 10, tolerance_type = "ppm", min_found = 3) # expect row 2
  filtered_5 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 222.1122, 333.218), tolerance = 5, tolerance_type = "ppm", min_found = 3) # expect empty tibble
  filtered_6 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 222.1122, 1000), tolerance = 10, tolerance_type = "ppm", min_found = 2) # expect row 2
  filtered_7 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 2000, 1000), tolerance = 10, tolerance_type = "ppm", min_found = 1) # expect row 2

  expect_equal(filtered_1[[1]][[1]], 1.1)
  expect_equal(nrow(filtered_2), 0)
  expect_equal(filtered_3[[1]][[1]], 1.1)
  expect_equal(filtered_4[[1]][[1]], 2.1)
  expect_equal(nrow(filtered_5), 0)
  expect_equal(filtered_6[[1]][[1]], 2.1)
  expect_equal(filtered_7[[1]][[1]], 2.1)
})

test_that("filters correctly for tolerance_type = 'absolute'", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble <- calc_neutral_loss(mgf_tibble, m_z_col = PEPMASS)

  filtered_1 <- filter_neutral_loss(mgf_tibble, losses = c(11.1111, 22.2222, 33.3333), tolerance = 0.00000000001, tolerance_type = "absolute", min_found = 3) # expect row 1
  filtered_2 <- filter_neutral_loss(mgf_tibble, losses = c(11.1111, 22.2222, 1000), tolerance = 0.00000000001, tolerance_type = "absolute", min_found = 3) # expect empty tibble
  filtered_3 <- filter_neutral_loss(mgf_tibble, losses = c(11.1111, 22.2222, 1000), tolerance = 0.00000000001, tolerance_type = "absolute", min_found = 2) # expect row 1

  filtered_4 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 222.1122, 333.220), tolerance = 0.003, tolerance_type = "absolute", min_found = 3) # expect row 2
  filtered_5 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 222.1122, 333.221), tolerance = 0.001, tolerance_type = "absolute", min_found = 3) # expect empty tibble
  filtered_6 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 222.1122, 1000), tolerance = 0.003, tolerance_type = "absolute", min_found = 2) # expect row 2
  filtered_7 <- filter_neutral_loss(mgf_tibble, losses = c(111.0111, 2000, 1000), tolerance = 0.003, tolerance_type = "absolute", min_found = 1) # expect row 2

  expect_equal(filtered_1[[1]][[1]], 1.1)
  expect_equal(nrow(filtered_2), 0)
  expect_equal(filtered_3[[1]][[1]], 1.1)
  expect_equal(filtered_4[[1]][[1]], 2.1)
  expect_equal(nrow(filtered_5), 0)
  expect_equal(filtered_6[[1]][[1]], 2.1)
  expect_equal(filtered_7[[1]][[1]], 2.1)
})

test_that("throws error if tolerance_type != c('absolute', 'ppm')", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble <- calc_neutral_loss(mgf_tibble, m_z_col = PEPMASS)

  expect_error(filter_neutral_loss(mgf_tibble, losses = c(12.345, 23.456, 34.567), tolerance = 0, tolerance_type = "absolutexxx", min_found = 3), 'Argument `tolerance_type` must be "ppm" or "absolute", not "absolutexxx".')
})
