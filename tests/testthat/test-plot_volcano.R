test_that("returns a ggplot if return_tbl = FALSE", {
  volc_plot <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = FALSE)

  expect_true(ggplot2::is.ggplot(volc_plot))
})

test_that("returns no ggplot if return_tbl = TRUE", {
  volc_plot <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = TRUE)

  expect_false(ggplot2::is.ggplot(volc_plot))
})

test_that("returns a ggplot if return_tbl = FALSE and no colors are provided", {
  volc_plot <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = FALSE, colors = NULL)

  expect_true(ggplot2::is.ggplot(volc_plot))
})

test_that("returns a no tibble if return_tbl = FALSE", {
  volc_plot <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = FALSE)

  expect_false(tibble::is_tibble(volc_plot))
})

test_that("returns a tibble if return_tbl = TRUE", {
  volc_tbl <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = TRUE, colors = NULL)

  expect_true(tibble::is_tibble(volc_tbl))
})

test_that("throws error if adjust_p method does not exist", {
  expect_error(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = FALSE, adjust_p = "asdf"))
})

test_that("throws error > 2 groups are provided", {
  expect_error(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment", "blank"), return_tbl = FALSE))
})

test_that("log2-fc is the same no matter if data are log2-transformed before of after calculating p-value", {
  l2_before_false <- toy_metaboscape %>% dplyr::mutate(Intensity = dplyr::case_when(is.na(Intensity)~1,.default = Intensity)) %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = TRUE) %>%
    dplyr::select(-"p_val", -"n_log_p_val")

  l2_before_true <- toy_metaboscape %>% dplyr::mutate(Intensity = dplyr::case_when(is.na(Intensity)~1,.default = Intensity)) %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = TRUE, log2_before = T) %>%
    dplyr::select(-"p_val", -"n_log_p_val")

  expect_equal(l2_before_false, l2_before_true)
})

test_that("returns the expected results for log2_before = FALSE", {
  calced_result <- test_plot_volcano_input %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = TRUE)

  expect_equal(calced_result$UID, test_plot_volcano_results$UID)
  expect_equal(calced_result$log2fc, test_plot_volcano_results$log2fc)
  expect_equal(calced_result$p_val, test_plot_volcano_results$p_val)
  expect_equal(calced_result$n_log_p_val, test_plot_volcano_results$n_log_p_val)
})

test_that("returns the expected results for log2_before = TRUE", {
  calced_result <- test_plot_volcano_input %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = TRUE, log2_before = TRUE)

  expect_equal(calced_result$UID, test_plot_volcano_results_log2_before$UID)
  expect_equal(calced_result$log2fc, test_plot_volcano_results_log2_before$log2fc)
  expect_equal(calced_result$p_val, test_plot_volcano_results_log2_before$p_val)
  expect_equal(calced_result$n_log_p_val, test_plot_volcano_results_log2_before$n_log_p_val)
})
