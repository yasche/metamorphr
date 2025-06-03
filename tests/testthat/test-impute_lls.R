test_that("order stays unchanged", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  before <- dplyr::select(mtData, -"Intensity")
  after <- mtData %>%
    impute_lls() %>%
    dplyr::select(-"Intensity")

  expect_equal(before, after)
})



test_that("gives the same results as pcaMethods::pca on a matrix with samples in rows and features in columns", {
  data("metaboliteData", package = "pcaMethods")

  mtData_tbl <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  mm_impute <- mtData_tbl %>%
    impute_lls()

  metaboliteData_t <- t(metaboliteData)

  pm_impute <- pcaMethods::llsImpute(metaboliteData_t)
  pm_impute <- pcaMethods::completeObs(pm_impute)

  pm_impute_t <- t(pm_impute)

  pm_impute_t <- pm_impute_t %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  expect_equal(mm_impute, pm_impute_t)
  expect_true("X0h" %in% rownames(metaboliteData_t))
  expect_true("Xylose methoxyamine (4TMS)" %in% colnames(metaboliteData_t))
})



test_that("Error if package pcaMethods is not installed", {
  local_mocked_bindings(
    is_installed_wrapper = function(pkg) {
      if (pkg == "pcaMethods") {
        FALSE
      } else {
        TRUE
      }
    },
    check_installed_wrapper = function(pkg) {
      if (pkg == "pcaMethods") {
        stop("")
      }
    }
  )

  expect_error(toy_metaboscape %>%
                 impute_lls(complete_genes = TRUE, cluster_size = 5))
})

test_that("Error if package impute and pak are not installed", {
  local_mocked_bindings(
    is_installed_wrapper = function(pkg) {
      if (pkg == "pcaMethods" | pkg == "pak") {
        FALSE
      } else {
        TRUE
      }
    },
    check_installed_wrapper = function(pkg) {
      if (pkg == "pcaMethods" | pkg == "pak") {
        stop("")
      }
    }
  )

  expect_error(toy_metaboscape %>%
                 impute_lls(complete_genes = TRUE, cluster_size = 5))
})

test_that('check_installed_wrapper("impute") is triggered', {
  local_mocked_bindings(
    is_installed_wrapper = function(pkg) {
      if (pkg == "pcaMethods" | pkg == "pak") {
        FALSE
      } else {
        TRUE
      }
    },
    check_installed_wrapper = function(pkg) {
      if (pkg == "pcaMethods") {
        stop("")
      }
    }
  )

  expect_error(toy_metaboscape %>%
                 impute_lls(complete_genes = TRUE, cluster_size = 5))
})

