test_that("throws error if tolerance_type != absolute or ppm", {
  expect_error(toy_metaboscape %>%
    filter_mz(m_z_col = `m/z`, tolerance_type = "absolutexxx"), 'Argument `tolerance_type` must be "ppm" or "absolute", not "absolutexxx".')
})

test_that("filters correctly for tolerance_type = 'ppm'", {
  # masses have 3 ppm deviation to correct ones:
  # c(162.1130, 305.2481, 238.0999) - c(162.1130, 305.2481, 238.0999) * 3/1000000

  filter_man <- toy_metaboscape %>%
    dplyr::filter(UID %in% c(1, 3, 7))

  filtered <- toy_metaboscape %>%
    filter_mz(m_z_col = `m/z`, masses = c(162.1125, 305.2472, 238.0992), tolerance = 5, tolerance_type = "ppm")

  expect_equal(filter_man, filtered)
})

test_that("filters correctly for tolerance_type = 'absolute'", {
  # masses have 0.003 Da deviation to correct ones:
  # c(162.1130, 305.2481, 238.0999) - 0.003
  filter_man <- toy_metaboscape %>%
    dplyr::filter(UID %in% c(1, 3, 7))

  filtered <- toy_metaboscape %>%
    filter_mz(m_z_col = `m/z`, masses = c(162.1100, 305.2451, 238.0969), tolerance = 0.005, tolerance_type = "absolute")

  expect_equal(filter_man, filtered)
})

test_that("works if variable is supplied", {
  # masses have 3 ppm deviation to correct ones:
  # c(162.1130, 305.2481, 238.0999) - c(162.1130, 305.2481, 238.0999) * 3/1000000

  search_masses <- c(162.1130, 305.2481, 238.0999, 418.2402)
  search_masses <- search_masses - search_masses * 3 / 1000000

  filter_man <- toy_metaboscape %>%
    dplyr::filter(UID %in% c(1, 3, 4, 7))

  filtered <- toy_metaboscape %>%
    filter_mz(m_z_col = `m/z`, masses = search_masses, tolerance = 5, tolerance_type = "ppm")

  expect_equal(filter_man, filtered)
})


test_that("works for syntactic and non-syntactic column names", {
  toy_mod <- toy_metaboscape %>%
    dplyr::mutate(mz = `m/z`)

  res_syn <- toy_mod %>%
    filter_mz(m_z_col = mz, masses = c(162.1100, 305.2451, 238.0969), tolerance = 0.005, tolerance_type = "absolute")

  res_non_syn <- toy_mod %>%
    filter_mz(m_z_col = `m/z`, masses = c(162.1100, 305.2451, 238.0969), tolerance = 0.005, tolerance_type = "absolute")

  expect_equal(res_syn, res_non_syn)
})


test_that("row & column order stays unchanged", {
  filtered_df <- toy_metaboscape %>%
    filter_mz(m_z_col = `m/z`, masses = 5000, tolerance = 5000, tolerance_type = "absolute")

  expect_equal(filtered_df, toy_metaboscape)
})
