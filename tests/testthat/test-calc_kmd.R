# tests for calc_kmd also cover calc_km and calc_nominal_km

test_that("KMD for homologues is approximately equal", {
  # measured masses are from https://pubs.acs.org/doi/10.1021/ac010560w
  oil_z <- c(351.3269,
             365.3425,
             379.3581,
             393.3738,
             407.3894,
             421.4051,
             435.4207,
             449.4364,
             463.4519,
             477.4676,
             491.4831,
             505.4989)
  oil_z_kmd <- round(calc_kmd(oil_z), digits = 2)

  oil_z_eq <- oil_z_kmd == oil_z_kmd[1]
  expect_true(all(oil_z_eq))
})


test_that("calc_kmd works with other repeating_units", {
  # measured masses are from https://pubs.acs.org/doi/10.1021/ac010560w
  oil_z <- c(351.3269,
             365.3425,
             379.3581,
             393.3738,
             407.3894,
             421.4051,
             435.4207,
             449.4364,
             463.4519,
             477.4676,
             491.4831,
             505.4989)
  oil_z_kmd_single <- calc_kmd(oil_z, repeating_unit = "CH2")
  oil_z_kmd_double <- calc_kmd(oil_z, repeating_unit = "(CH2)2")

  expect_equal(oil_z_kmd_single, oil_z_kmd_double)
})
