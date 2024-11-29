test_that("works for min_frac = 5", {
  filtered_features <- test_filters %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3"))) %>%
    filter_blank(min_frac = 5, blank_samples = "b1") %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f10"))
})

test_that("works for min_frac = 3", {
  filtered_features <- test_filters %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3"))) %>%
    filter_blank(min_frac = 3, blank_samples = "b1") %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f10"))
})

test_that("works for min_frac = 2", {
  filtered_features <- test_filters %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3"))) %>%
    filter_blank(min_frac = 2, blank_samples = "b1") %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f10", "f11"))
})

test_that("works for min_frac = 1.5", {
  filtered_features <- test_filters %>%
    #only look at "samples"
    dplyr::filter(!(Sample %in% c("q1", "q2", "q3"))) %>%
    filter_blank(min_frac = 1.5, blank_samples = "b1") %>%
    dplyr::select(2) %>%
    dplyr::pull() %>%
    unique()

  expect_equal(filtered_features, c("f1", "f2", "f3", "f4", "f5", "f10", "f11"))
})

test_that("does not change the structure of the input in an unexpected way", {
  filtered_features <- test_filters %>%
    #only look at "samples"
    filter_blank(min_frac = 0, blank_samples = "b1")

  expect_equal(filtered_features, test_filters)
})

test_that("does not change the structure of the input in an unexpected way", {
  test_tibble <- tibble::tibble(UID = c(1,1),
                                Feature = c("f0", "f0"),
                                Sample = c("s", "b"),
                                Intensity = c(0, 0))

  filtered_features <- test_tibble %>%
    #only look at "samples"
    filter_blank(min_frac = 0, blank_samples = "b")

  expect_equal(nrow(filtered_features), 0)
})
