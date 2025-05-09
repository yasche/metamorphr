test_that("normalize_quantile_smooth produces expected results, when accounting for rounding error", {
  #Values in the publication used for this evaluation are rounded to the 2nd digit (Y. Zhao, L. Wong, W. W. B. Goh, Sci Rep 2020, 10, 15534, DOI 10.1038/s41598-020-72664-6)
  #R may round differently, see ?round
  #therefore, differences between expected results and calculated results are first calculated.
  #As numbers are rounded to the 2nd digit, absolute differences should be less than or equal to 0.005.
  max_diff <- test_qn_data %>%
    join_metadata(test_qn_metadata) %>%
    normalize_quantile_smooth(group_column = .data$Group) %>%
    dplyr::select(-"Feature") %>%
    dplyr::left_join(test_qn_smooth_results, by = c("UID", "Sample")) %>%
    dplyr::mutate(Intenity_diff = .data$Intensity.x - .data$Intensity.y) %>%
    dplyr::pull(.data$Intenity_diff) %>%
    abs() %>%
    max()

  expect_lte(max_diff, 0.005)
})

test_that("normalize_quantile_smooth produces expected results, when accounting for rounding error; standard arguments", {
  #Values in the publication used for this evaluation are rounded to the 2nd digit (Y. Zhao, L. Wong, W. W. B. Goh, Sci Rep 2020, 10, 15534, DOI 10.1038/s41598-020-72664-6)
  #R may round differently, see ?round
  #therefore, differences between expected results and calculated results are first calculated.
  #As numbers are rounded to the 2nd digit, absolute differences should be less than or equal to 0.005.
  max_diff <- test_qn_data %>%
    join_metadata(test_qn_metadata) %>%
    normalize_quantile_smooth() %>%
    dplyr::select(-"Feature") %>%
    dplyr::left_join(test_qn_smooth_results, by = c("UID", "Sample")) %>%
    dplyr::mutate(Intenity_diff = .data$Intensity.x - .data$Intensity.y) %>%
    dplyr::pull(.data$Intenity_diff) %>%
    abs() %>%
    max()

  expect_lte(max_diff, 0.005)
})

test_that("error if rolling_window < 0", {
  expect_error(
    test_qn_data %>%
      join_metadata(test_qn_metadata) %>%
      normalize_quantile_smooth(group_column = .data$Group, rolling_window = -1)
  )
})

test_that("error if rolling_window > 1", {
  expect_error(
    test_qn_data %>%
      join_metadata(test_qn_metadata) %>%
      normalize_quantile_smooth(group_column = .data$Group, rolling_window = 2)
  )
})

test_that("normalize_quantile_smooth returns the same values as qsmooth::qsmooth()", {
  data(khanmiss, package = "impute")

  #prepare data and metadata

  khan_group <- khanmiss[1, -(1:2)] %>%
    t() %>%
    c()

  khan_sample <- khanmiss[1, -(1:2)] %>%
    t() %>%
    rownames()

  khan_group <- tibble::tibble(
    Sample = khan_sample,
    Group = khan_group
  )

  khan_tidy <- khanmiss[-1, -(1:2)] %>%
    as.matrix() %>%
    tibble::as_tibble() %>%
    purrr::map(as.numeric) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(UID = seq(1, length(.data$sample1))) %>%
    tidyr::gather(-UID, key = "Sample", value = "Intensity") %>%
    dplyr::mutate(Intensity = exp(.data$Intensity)) %>%
    #some form of MVI must be performed
    metamorphr::impute_lod()

  khan_wide <- khan_tidy %>%
    tidyr::spread(key = Sample, value = Intensity) %>%
    dplyr::select(-UID) %>%
    dplyr::relocate(khan_group$Sample)

  khan_tidy <- khan_tidy %>%
    metamorphr::join_metadata(khan_group)

  #perform normalization and some wrangling
  qs_results <- qsmooth::qsmooth(khan_wide, group_factor = khan_group$Group)
  qs_results <- qs_results@qsmoothData

  mm_results <- metamorphr::normalize_quantile_smooth(khan_tidy, group_column = Group) %>%
    dplyr::select(-Group) %>%
    tidyr::spread(key = Sample, value = Intensity) %>%
    dplyr::select(-UID) %>%
    dplyr::relocate(colnames(qs_results)) %>%
    as.matrix()


  expect_equal(qs_results, mm_results)
})

