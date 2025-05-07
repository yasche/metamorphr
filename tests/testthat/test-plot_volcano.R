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
