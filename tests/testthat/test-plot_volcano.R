test_that("returns a ggplot if return_tbl = FALSE", {
  volc_plot <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = FALSE)

  expect_true(ggplot2::is.ggplot(volc_plot))
})

test_that("throws error if adjust_p method does not exist", {
  expect_error(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment"), return_tbl = FALSE, adjust_p = "asdf"))
})

test_that("throws error > 2 groups are provided", {
  expect_error(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% plot_volcano(group_column = Group, groups_to_compare = c("control", "treatment", "blank"), return_tbl = FALSE))
})
