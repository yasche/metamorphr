test_that("order stays unchanged, direction = 1", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  before <- dplyr::select(mtData, -"Intensity")
  after <- mtData %>%
    impute_nipals(direction = 1) %>%
    dplyr::select(-"Intensity")

  expect_equal(before, after)
})

test_that("order stays unchanged, direction = 2", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  before <- dplyr::select(mtData, -"Intensity")
  after <- mtData %>%
    impute_nipals(direction = 2) %>%
    dplyr::select(-"Intensity")

  expect_equal(before, after)
})



test_that("direction = 1 gives the same results as pcaMethods::pca on a matrix with samples in columns and features in rows", {
  data("metaboliteData", package = "pcaMethods")

  mtData_tbl <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  mm_impute <- mtData_tbl %>%
    impute_nipals(direction = 1)


  pm_impute <- pcaMethods::pca(metaboliteData, nPcs = 2, method = "nipals")
  pm_impute <- pcaMethods::completeObs(pm_impute)

  pm_impute <- pm_impute %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  expect_equal(mm_impute, pm_impute)
  expect_true("Xylose methoxyamine (4TMS)" %in% rownames(metaboliteData))
  expect_true("X0h" %in% colnames(metaboliteData))
})

test_that("direction = 2 gives the same results as pcaMethods::pca on a matrix with samples in rows and features in columns", {
  data("metaboliteData", package = "pcaMethods")

  mtData_tbl <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  mm_impute <- mtData_tbl %>%
    impute_nipals(direction = 2)

  metaboliteData_t <- t(metaboliteData)

  pm_impute <- pcaMethods::pca(metaboliteData_t, nPcs = 2, method = "nipals")
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
                 impute_nipals())
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
                 impute_nipals())
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
                 impute_nipals())
})
