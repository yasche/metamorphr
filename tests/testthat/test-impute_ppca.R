test_that("order stays unchanged, direction = 1", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  before <- dplyr::select(mtData, -"Intensity")
  after <- mtData %>%
    impute_ppca(direction = 1, random_seed = 1L) %>%
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
    impute_ppca(direction = 2, random_seed = 1L) %>%
    dplyr::select(-"Intensity")

  expect_equal(before, after)
})



test_that("random seed is unchanged, direction = 1", {
  seed_before <- .Random.seed

  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature)) %>%
    impute_ppca(direction = 1, random_seed = 1L)

  seed_after <- .Random.seed

  expect_equal(seed_before, seed_after)
})


test_that("random seed is unchanged, direction = 2", {
  seed_before <- .Random.seed

  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature)) %>%
    impute_ppca(direction = 2, random_seed = 1L)

  seed_after <- .Random.seed

  expect_equal(seed_before, seed_after)
})



test_that("result is reproducible if seed is provided, direction = 1", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))


  impute1 <- mtData %>%
    impute_ppca(direction = 1, random_seed = 1L)

  impute2 <- mtData %>%
    impute_ppca(direction = 1, random_seed = 1L)

  expect_equal(impute1, impute2)
})


test_that("result is reproducible if seed is provided, direction = 2", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))


  impute1 <- mtData %>%
    impute_ppca(direction = 2, random_seed = 1L)

  impute2 <- mtData %>%
    impute_ppca(direction = 2, random_seed = 1L)

  expect_equal(impute1, impute2)
})


test_that("result differs for different seeds, direction = 1", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))


  impute1 <- mtData %>%
    impute_ppca(direction = 1, random_seed = 1L)

  impute2 <- mtData %>%
    impute_ppca(direction = 1, random_seed = 2L)

  expect_true(any(impute1$Intensity != impute2$Intensity))
})


test_that("result differs for different seeds, direction = 2", {
  data("metaboliteData", package = "pcaMethods")

  mtData <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))


  impute1 <- mtData %>%
    impute_ppca(direction = 2, random_seed = 1L)

  impute2 <- mtData %>%
    impute_ppca(direction = 2, random_seed = 2L)

  expect_true(any(impute1$Intensity != impute2$Intensity))
})




test_that("direction = 1 gives the same results as pcaMethods::pca on a matrix with samples in columns and features in rows", {
  data("metaboliteData", package = "pcaMethods")

  mtData_tbl <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  mm_impute <- mtData_tbl %>%
    impute_ppca(direction = 1, random_seed = 1)

  #the column order has an effect on the result!
  #for comparability reasons, columns in the matrix have to be ordered the same way, they are ordered when using impute_ppca
  metaboliteData_colorder <- metaboliteData[,c("X0h","X0h.1","X0h.2","X0h.3","X0h.4","X0h.5","X0h.6","X12h","X12h.1","X12h.2","X12h.3","X12h.4","X12h.5","X12h.6","X12h.7","X1h","X1h.1","X1h.2","X1h.3","X1h.4", "X1h.5","X1h.6","X1h.7","X24h","X24h.1","X24h.2","X24h.3","X24h.4","X24h.5","X24h.6","X48h","X48h.1","X48h.2","X48h.3","X48h.4","X48h.5","X48h.6","X48h.7","X4h","X4h.1","X4h.2", "X4h.3","X4h.4","X4h.5","X96h","X96h.1","X96h.2","X96h.3","X96h.4","X96h.5","X96h.6","X96h.7")]

  pm_impute <- withr::with_preserve_seed(pcaMethods::pca(metaboliteData_colorder, nPcs = 2, method = "ppca", seed = 1))
  pm_impute <- pcaMethods::completeObs(pm_impute)

  pm_impute <- pm_impute %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))


  pm_impute <- dplyr::arrange(pm_impute, Sample, UID)
  mm_impute <- dplyr::arrange(mm_impute, Sample, UID)

  expect_equal(mm_impute, pm_impute)
  expect_true("Xylose methoxyamine (4TMS)" %in% rownames(metaboliteData))
  expect_true("X0h" %in% colnames(metaboliteData))
})


test_that("control for the test above; column order has an influence", {
  data("metaboliteData", package = "pcaMethods")

  mtData_tbl <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  mm_impute <- mtData_tbl %>%
    impute_ppca(direction = 1, random_seed = 1)


  pm_impute <- withr::with_preserve_seed(pcaMethods::pca(metaboliteData, nPcs = 2, method = "ppca", seed = 1))
  pm_impute <- pcaMethods::completeObs(pm_impute)

  pm_impute <- pm_impute %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))


  pm_impute <- dplyr::arrange(pm_impute, Sample, UID)
  mm_impute <- dplyr::arrange(mm_impute, Sample, UID)

  expect_true(any(mm_impute$Intensity != pm_impute$Intensity))
})



test_that("direction = 2 gives the same results as pcaMethods::pca on a matrix with samples in rows and features in columns", {
  data("metaboliteData", package = "pcaMethods")

  mtData_tbl <- metaboliteData %>%
    tibble::as_tibble(rownames = "Feature") %>%
    dplyr::mutate(UID = 1:nrow(.)) %>%
    tidyr::gather(key = "Sample", value = "Intensity", -c(UID, Feature))

  mm_impute <- mtData_tbl %>%
    impute_ppca(direction = 2, random_seed = 1)

  metaboliteData_t <- t(metaboliteData)

  pm_impute <- pcaMethods::pca(metaboliteData_t, nPcs = 2, method = "ppca", seed = 1)
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
                 impute_ppca())
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
                 impute_ppca())
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
                 impute_ppca())
})

