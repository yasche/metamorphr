test_that(".Random.seed stays untouched with impute_knn", {
  # state of the random number generator should not be changed
  # described here https://r-pkgs.org/code.html#sec-code-r-landscape

  set.seed(1)

  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    # dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  # print(khan.expr)
  seed_before <- .Random.seed
  khan_imputed <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 123, rowmax = 1)
  seed_after <- .Random.seed

  expect_true(all(seed_before == seed_after))
})

test_that(".Random.seed changes with impute.knn", {
  set.seed(1)

  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)]

  seed_before <- .Random.seed
  khan.imputed <- impute::impute.knn(as.matrix(khan.expr), rng.seed = 123, rowmax = 1)
  seed_after <- .Random.seed


  expect_false(all(seed_before == seed_after))
})


test_that("impute.knn and impute_knn give the same results", {
  # prepare data for impute_knn
  data(khanmiss, package = "impute")

  khan_impute_knn <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    # dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  khan_impute_knn_imputed <- impute_knn(khan_impute_knn, quietly = TRUE) %>%
    # data from impute.knn is still ln-transformed
    dplyr::mutate(Intensity = log(.data$Intensity)) %>%
    tidyr::spread(key = "Sample", value = "Intensity") %>%
    dplyr::select(-UID)

  khan_impute.knn <- khanmiss[-1, -(1:2)] %>%
    as.matrix()

  khan_impute.knn_imputed <- impute::impute.knn(khan_impute.knn)

  khan_impute.knn_imputed <- khan_impute.knn_imputed$data %>%
    tidyr::as_tibble()

  # restore column order
  khan_impute_knn_imputed <- khan_impute_knn_imputed[colnames(khan_impute.knn_imputed)] %>%
    tidyr::as_tibble()

  expect_equal(khan_impute_knn_imputed, khan_impute.knn_imputed)
})

# test_that("impute_knn does not alter the input", {
#
# })

test_that("impute_knn is reproducible if random seed is provided", {
  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    # dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  imputed_1 <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 123)
  imputed_2 <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 123)
  imputed_3 <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 123)

  expect_true(all(imputed_1 == imputed_2))
  expect_true(all(imputed_1 == imputed_3))
})

test_that("control for previous test", {
  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    # dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  imputed_1 <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 123)
  imputed_2 <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 456)
  imputed_3 <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 789)

  expect_false(all(imputed_1 == imputed_2))
  expect_false(all(imputed_1 == imputed_3))
})

test_that("quiet = TRUE and quiet = FALSE produce the same results", {
  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))


  khan_imputed_t <- impute_knn(khan.expr, quietly = TRUE, rng.seed = 123, rowmax = 1)
  khan_imputed_f <- impute_knn(khan.expr, quietly = FALSE, rng.seed = 123, rowmax = 1)

  expect_equal(khan_imputed_t, khan_imputed_f)
})

test_that("wanrings are printed if quietly = TRUE", {
  expect_warning(impute_knn(toy_metaboscape, quietly = F))
  expect_warning(impute_knn(toy_metaboscape, quietly = T))
})

test_that("correct number of samples", {
  expect_error(rlang::check_installed("avc"))
})


test_that("Error if package impute is not installed", {
  local_mocked_bindings(
    is_installed_wrapper = function(pkg) {
      if (pkg == "impute") {
        FALSE
      } else {
        TRUE
      }
    },
    check_installed_wrapper = function(pkg) {
      if (pkg == "impute") {
        stop("")
      }
    }
  )

  expect_error(impute_knn(toy_metaboscape))
})

test_that("Error if package impute and pak are not installed", {
  local_mocked_bindings(
    is_installed_wrapper = function(pkg) {
      if (pkg == "impute" | pkg == "pak") {
        FALSE
      } else {
        TRUE
      }
    },
    check_installed_wrapper = function(pkg) {
      if (pkg == "impute" | pkg == "pak") {
        stop("")
      }
    }
  )

  expect_error(impute_knn(toy_metaboscape))
})

test_that('check_installed_wrapper("impute") is triggered', {
  local_mocked_bindings(
    is_installed_wrapper = function(pkg) {
      if (pkg == "impute" | pkg == "pak") {
        FALSE
      } else {
        TRUE
      }
    },
    check_installed_wrapper = function(pkg) {
      if (pkg == "impute") {
        stop("")
      }
    }
  )

  expect_error(impute_knn(toy_metaboscape))
})

test_that("test check_installed_wrapper function", {
  expect_error(check_installed_wrapper("abc"))
})
