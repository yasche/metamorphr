test_that("gives message if verbose = T and none if verbose = F", {
  expect_no_message(toy_metaboscape %>% impute_lod() %>% normalize_cyclic_loess(verbose = F))
  expect_message(toy_metaboscape %>% impute_lod() %>% normalize_cyclic_loess(verbose = T))
})

test_that("runs correct number of iterations", {
  expect_message(toy_metaboscape %>% impute_lod() %>% normalize_cyclic_loess(verbose = T, fixed_iter = T, n_iter = 3), "Successfully ran 3 iterations.")
})


test_that("medians of samples become more similar after each iteration (3)", {
  rsd_summary_median <- function(data) {
    medians <- data %>%
      summary_featuretable() %>%
      purrr::map(`[`, "Median") %>%
      unlist() %>%
      unname()

    sd(medians) / mean(medians)
  }

  toy_imputed_lod <- impute_lod(toy_metaboscape)

  #before > after 1 iteration
  expect_true(rsd_summary_median(toy_imputed_lod) > rsd_summary_median(normalize_cyclic_loess(toy_imputed_lod, fixed_iter = T, n_iter = 1)))
  #after 1 iteration > after 2 iterations
  expect_true(rsd_summary_median(normalize_cyclic_loess(toy_imputed_lod, fixed_iter = T, n_iter = 1)) > rsd_summary_median(normalize_cyclic_loess(toy_imputed_lod, fixed_iter = T, n_iter = 2)))
  #after 2 iteration > after 3 iterations
  expect_true(rsd_summary_median(normalize_cyclic_loess(toy_imputed_lod, fixed_iter = T, n_iter = 2)) > rsd_summary_median(normalize_cyclic_loess(toy_imputed_lod, fixed_iter = T, n_iter = 3)))
})


test_that("automatically stops if convergence is reached", {
  expect_message(toy_metaboscape %>% impute_lod() %>% normalize_cyclic_loess(verbose = T, fixed_iter = F, n_iter = 10), "Reached convergence")
})


test_that("breaks if no convergence is reached", {
  expect_message(toy_metaboscape %>% impute_lod() %>% normalize_cyclic_loess(verbose = T, fixed_iter = F, n_iter = 1), "No convergence was reached after 1 iterations.")
})

test_that("row & column order stays unchanged", {
  normalized_df <- toy_metaboscape %>%
    impute_lod() %>%
    normalize_cyclic_loess(verbose = F, n_iter = 1)

  expect_equal(dplyr::select(normalized_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
