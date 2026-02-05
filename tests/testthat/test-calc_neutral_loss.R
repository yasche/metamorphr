test_that("row & column order stays unchanged", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble_nl <- msn_calc_nl(mgf_tibble, m_z_col = PEPMASS)

  expect_equal(dplyr::select(mgf_tibble_nl, -"Neutral_Loss"), mgf_tibble)
})

test_that("neutral_losses are correct", {
  nl1 <- tibble::tribble(
    ~m_z, ~Intensity,
    33.3333,1,
    22.2222,2,
    11.1111,3)

  nl2 <- tibble::tribble(
    ~m_z, ~Intensity,
    555.4445,4,
    444.3334,5,
    333.2223,6,
    222.1122,7,
    111.0111,8)

  nl3 <- tibble::tribble(
    ~m_z, ~Intensity,
    4655.5556,9,
    3544.4445,10,
    2433.3344,11,
    1322.2333,12,
    211.2222,13)

  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble_nl <- msn_calc_nl(mgf_tibble, m_z_col = PEPMASS) %>%
    dplyr::pull("Neutral_Loss")

  expect_equal(mgf_tibble_nl[[1]], nl1)
  expect_equal(mgf_tibble_nl[[2]], nl2)
  expect_equal(mgf_tibble_nl[[3]], nl3)
})

test_that("returns NULL if no MSn spectrum is available", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble$MSn[1] <- list(NULL)
  mgf_tibble_nl <- msn_calc_nl(mgf_tibble, m_z_col = PEPMASS)

  expect_null(mgf_tibble$MSn[[1]])
  expect_null(mgf_tibble_nl$Neutral_Loss[[1]])
})

test_that("returns NULL if Neutral_Loss tibble is empty", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble$PEPMASS[1] <- 1
  mgf_tibble_nl <- msn_calc_nl(mgf_tibble, m_z_col = PEPMASS)

  expect_null(mgf_tibble_nl$Neutral_Loss[[1]])
})

test_that("deprecation warning when calling old function", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))

  expect_warning(calc_neutral_loss(mgf_tibble, m_z_col = PEPMASS))
})
