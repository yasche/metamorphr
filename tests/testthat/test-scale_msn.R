test_that("row & column order stays unchanged", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble_scale <- scale_msn(mgf_tibble)

  expect_equal(rownames(mgf_tibble_scale), rownames(mgf_tibble))
  expect_equal(colnames(mgf_tibble_scale), colnames(mgf_tibble))
})

test_that("returns NULL if no MSn spectrum is available", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))
  mgf_tibble$MSn[1] <- list(NULL)
  mgf_tibble_scale <- scale_msn(mgf_tibble)

  expect_null(mgf_tibble$MSn[[1]])
  expect_null(mgf_tibble_scale$MSn[[1]])
})

test_that("scaling works as expected for some scaling factors", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))

  exp_scale <- list(
    tibble::tribble(
      ~m_z, ~Intensity,
      12.3456, 1/3,
      23.4567, 2/3,
      34.5678, 3/3
    ),
    tibble::tribble(
      ~m_z, ~Intensity,
      123.4567, 4/8,
      234.5678, 5/8,
      345.6789, 6/8,
      456.7890, 7/8,
      567.8901, 8/8
    ),
    tibble::tribble(
      ~m_z, ~Intensity,
      1234.5678, 9/14,
      2345.6789, 10/14,
      3456.7890, 11/14,
      4567.8901, 12/14,
      5678.9012, 13/14,
      6789.0123, 14/14
    )
  )

  exp_scale_100 <- purrr::map(exp_scale, dplyr::mutate, Intensity = Intensity * 100)
  exp_scale_1000 <- purrr::map(exp_scale, dplyr::mutate, Intensity = Intensity * 1000)
  exp_scale_5 <- purrr::map(exp_scale, dplyr::mutate, Intensity = Intensity * 5)
  exp_scale_1 <- purrr::map(exp_scale, dplyr::mutate, Intensity = Intensity * 1)

  mgf_tibble_scale_100 <- scale_msn(mgf_tibble, scale_to = 100)
  mgf_tibble_scale_1000 <- scale_msn(mgf_tibble, scale_to = 1000)
  mgf_tibble_scale_5 <- scale_msn(mgf_tibble, scale_to = 5)
  mgf_tibble_scale_1 <- scale_msn(mgf_tibble, scale_to = 1)

  expect_equal(mgf_tibble_scale_100$MSn, exp_scale_100)
  expect_equal(mgf_tibble_scale_1000$MSn, exp_scale_1000)
  expect_equal(mgf_tibble_scale_5$MSn, exp_scale_5)
  expect_equal(mgf_tibble_scale_1$MSn, exp_scale_1)
})


test_that("highest number is correct for some scaling factors", {
  mgf_tibble <- read_mgf(test_path("data", "test_filter_msn_nl.mgf"))


  max_mgf_tibble_scale_100 <- scale_msn(mgf_tibble, scale_to = 100)$MSn %>%
    purrr::transpose() %>%
    `[[`("Intensity") %>%
    purrr::map(max) %>%
    unlist()

  max_mgf_tibble_scale_1000 <- scale_msn(mgf_tibble, scale_to = 1000)$MSn %>%
    purrr::transpose() %>%
    `[[`("Intensity") %>%
    purrr::map(max) %>%
    unlist()

  max_mgf_tibble_scale_5 <- scale_msn(mgf_tibble, scale_to = 5)$MSn %>%
    purrr::transpose() %>%
    `[[`("Intensity") %>%
    purrr::map(max) %>%
    unlist()

  max_mgf_tibble_scale_1 <- scale_msn(mgf_tibble, scale_to = 1)$MSn %>%
    purrr::transpose() %>%
    `[[`("Intensity") %>%
    purrr::map(max) %>%
    unlist()


  expect_equal(max_mgf_tibble_scale_100, rep(100, nrow(mgf_tibble)))
  expect_equal(max_mgf_tibble_scale_1000, rep(1000, nrow(mgf_tibble)))
  expect_equal(max_mgf_tibble_scale_5, rep(5, nrow(mgf_tibble)))
  expect_equal(max_mgf_tibble_scale_1, rep(1, nrow(mgf_tibble)))
})
