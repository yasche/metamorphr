test_that("Column names are correct", {
  expect_equal(
    colnames(read_mgf(test_path("data", "test_read_mgf.mgf"))),
    c("VARIABLEONE", "VARIABLETWO", "VARIABLETHREE", "MSn")
  )
})

test_that("tab and space separated values produce the same result", {
  expect_equal(
    read_mgf(test_path("data", "test_read_mgf.mgf")),
    read_mgf(test_path("data", "test_read_mgf_tab.mgf"))
  )
})

test_that("Observations in columns are correct", {
  mgf_tibble <- read_mgf(test_path("data", "test_read_mgf.mgf"))
  col1 <- mgf_tibble[[1]]
  col2 <- mgf_tibble[[2]]
  col3 <- mgf_tibble[[3]]

  expect_equal(col1, c(1.1, 2.1, 3.1))
  expect_equal(col2, c(1.2, 2.2, 3.2))
  expect_equal(col3, c(1.3, 2.3, 3.3))
})

test_that("MSn spectra are correct", {
  mgf_tibble <- read_mgf(test_path("data", "test_read_mgf.mgf"))

  msspec1 <- tibble::tribble(~m_z, ~Intensity,
                             12.345,1,
                             23.456,2,
                             34.567,3)

  msspec2 <- tibble::tribble(~m_z, ~Intensity,
                             123.456,4,
                             234.567,5,
                             345.678,6,
                             456.789,7,
                             567.890,8)

  msspec3 <- tibble::tribble(~m_z, ~Intensity,
                             1234.567,9,
                             2345.678,10,
                             3456.789,11,
                             4567.890,12,
                             5678.901,13,
                             6789.012,14)

  expect_equal(mgf_tibble$MSn[[1]], msspec1)
  expect_equal(mgf_tibble$MSn[[2]], msspec2)
  expect_equal(mgf_tibble$MSn[[3]], msspec3)
})

