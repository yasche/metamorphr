test_that(".Random.seed stays untouched with impute_rf, seed provided", {
  # state of the random number generator should not be changed
  # described here https://r-pkgs.org/code.html#sec-code-r-landscape

  set.seed(12345)

  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    #only use small subset to save CPU time
    head(50) %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    # dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  # print(khan.expr)
  seed_before <- .Random.seed
  khan_imputed <- impute_rf(khan.expr, maxiter = 1, ntree = 2)
  seed_after <- .Random.seed

  expect_true(all(seed_before == seed_after))
})

test_that(".Random.seed stays untouched with impute_rf, no seed provided", {
  # state of the random number generator should not be changed
  # described here https://r-pkgs.org/code.html#sec-code-r-landscape

  set.seed(12345)

  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    #only use small subset to save CPU time
    head(50) %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    # dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  # print(khan.expr)
  seed_before <- .Random.seed
  khan_imputed <- impute_rf(khan.expr, random_seed = NULL, maxiter = 1, ntree = 2)
  seed_after <- .Random.seed

  expect_true(all(seed_before == seed_after))
})

test_that("No more NA after imputation", {
  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    #only use small subset to save CPU time
    head(50) %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    dplyr::mutate(Intensity = exp(.data$Intensity))

  na_before <- NA %in% khan.expr$Intensity

  khan_imputed <- impute_rf(khan.expr, random_seed = NULL, maxiter = 1, ntree = 2)

  na_after <- NA %in% khan_imputed$Intensity

  expect_true(na_before)
  expect_false(na_after)
})
