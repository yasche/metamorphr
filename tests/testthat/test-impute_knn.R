test_that(".Random.seed stays untouched with impute_knn", {
  #state of the random number generator should not be changed
  #described here https://r-pkgs.org/code.html#sec-code-r-landscape

  set.seed(1)

  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    #dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    dplyr::mutate(Intensity = exp(Intensity))

  #print(khan.expr)
  seed_before <- .Random.seed
  khan_imputed <-  impute_knn(khan.expr, rng.seed = 123, rowmax = 1)
  seed_after <- .Random.seed

  expect_true(all(seed_before == seed_after))
})

test_that(".Random.seed changes with impute.knn", {
  set.seed(2)

  data(khanmiss, package = "impute")
  khan.expr <- khanmiss[-1, -(1:2)]

  seed_before <- .Random.seed
  khan.imputed <- impute::impute.knn(as.matrix(khan.expr), rng.seed = 123, rowmax = 1)
  seed_after <- .Random.seed


  expect_false(all(seed_before == seed_after))
})

#test khan expr gives the same result
