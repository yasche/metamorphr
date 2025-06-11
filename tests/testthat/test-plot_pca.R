test_that("returns a ggplot for default parameters", {
  p <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca()
  expect_true(ggplot2::is_ggplot(p))
})

test_that('returns a ggplot for what = "scores"', {
  p <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca(what = "scores")
  expect_true(ggplot2::is_ggplot(p))
})

test_that('returns a ggplot for what = "loadings"', {
  p <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca(what = "loadings")
  expect_true(ggplot2::is_ggplot(p))
})

test_that('returns a tbl for what = "scores" & return_tbl = TRUE', {
  tib <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca(what = "scores", return_tbl = TRUE)
  expect_true(tibble::is_tibble(tib))
})

test_that('returns a ggplot for what = "loadings" & return_tbl = TRUE', {
  tib <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca(what = "loadings", return_tbl = TRUE)
  expect_true(tibble::is_tibble(tib))
})

test_that("throws error if n_pcs < 1", {
  expect_error(toy_metaboscape %>%
                 join_metadata(toy_metaboscape_metadata) %>%
                 impute_lod() %>%
                 plot_pca(n_pcs = 0, pcs = c(2,3)))
})

test_that("runs correct pca if verbose = TRUE for data without NA", {
  expect_output(toy_metaboscape %>%
                  join_metadata(toy_metaboscape_metadata) %>%
                  impute_lod() %>%
                  plot_pca(verbose = TRUE), "svd calculated PCA")
})

test_that("runs correct pca if verbose = TRUE for data with NA", {
  expect_warning(toy_metaboscape %>%
                  join_metadata(toy_metaboscape_metadata) %>%
                  plot_pca(verbose = TRUE), "data has missing values using nipals instead of user requested svd")
})

test_that("tbl with scores data contains Group column if group_column is provided", {
  tib <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca(what = "scores", return_tbl = TRUE, group_column = Group)

  expect_true("Group" %in% colnames(tib))
})

test_that("tbl with loadings data contains Feature column if name_column is provided", {
  tib <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    plot_pca(what = "loadings", return_tbl = TRUE, name_column = Feature)

  expect_true("Feature" %in% colnames(tib))
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
                 join_metadata(toy_metaboscape_metadata) %>%
                 impute_lod() %>%
                 plot_pca())
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
                 join_metadata(toy_metaboscape_metadata) %>%
                 impute_lod() %>%
                 plot_pca())
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
                 join_metadata(toy_metaboscape_metadata) %>%
                 impute_lod() %>%
                 plot_pca())
})

test_that("Control for tests above (with local_mocked_bindings)", {
  expect_true(ggplot2::is_ggplot(toy_metaboscape %>%
                                   join_metadata(toy_metaboscape_metadata) %>%
                                   impute_lod() %>%
                                   plot_pca()))
})

test_that("ggplot object if group_column is provided", {
  expect_true(ggplot2::is_ggplot(toy_metaboscape %>%
                                   join_metadata(toy_metaboscape_metadata) %>%
                                   impute_lod() %>%
                                   plot_pca(group_column = Group)))
})
