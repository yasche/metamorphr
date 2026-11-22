test_that("filtering does not affect tibble if done correctly", {
  volc_tbl_filtered <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  batch_column = Batch,
                  batch = 1)

  volc_tbl_not_filtered <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"))

  expect_equal(volc_tbl_filtered, volc_tbl_not_filtered)
})


test_that("returns an empty tibble if batch does not exist and rt_col is NULL; filtering works", {
  tbl <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  batch_column = Batch,
                  batch = 2)

  expect_true(nrow(tbl) == 0)
  expect_equal(colnames(tbl), c("m.z", "p.value", "t.score"))
})

test_that("returns an empty tibble if batch does not exist and rt_col is provided; filtering works", {
  tbl <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                 m_z_col = `m/z`,
                 groups_to_compare = c("control", "treatment"),
                 batch_column = Batch,
                 batch = 2,
                 rt_col = RT)

  expect_true(nrow(tbl) == 0)
  expect_equal(colnames(tbl), c("m.z", "p.value", "t.score", "r.t"))
})


test_that("has correct column names if rt_col is not provided; returns non-empty df", {
  tbl <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"))

  expect_true(nrow(tbl) > 0)
  expect_equal(colnames(tbl), c("m.z", "p.value", "t.score"))
})

test_that("has correct column names if rt_col is provided; returns non-empty df", {
  tbl <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  rt_col = RT)

  expect_true(nrow(tbl) > 0)
  expect_equal(colnames(tbl), c("m.z", "p.value", "t.score", "r.t"))
})


test_that("p.values, adjusted p.values and t.scores contain non-NA values; adjusted and non-adjusted p.values are different", {
  tbl_non_adj <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  rt_col = RT,
                  adjust_p = FALSE)

  tbl_adj <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  rt_col = RT,
                  adjust_p = "fdr")

  expect_false(isTRUE(all.equal(tbl_non_adj$p.value, tbl_adj$p.value)))
  expect_true(any(!is.na(tbl_non_adj$p.value)))
  expect_true(any(!is.na(tbl_adj$p.value)))
  expect_true(any(!is.na(tbl_non_adj$t.score)))
  expect_true(any(!is.na(tbl_adj$t.score)))
  #tbl_non_adj
  #tbl_non_adj
})

test_that("tibble has correct dimensions", {
  tbl_rt_non_adj <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  rt_col = RT,
                  adjust_p = FALSE)

  tbl_rt_adj <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  rt_col = RT,
                  adjust_p = "fdr")

  tbl_non_rt_filtered <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  batch_column = Batch,
                  batch = 1)

  tbl_rt_filtered <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    convert_to_fa(group_column = Group,
                  m_z_col = `m/z`,
                  groups_to_compare = c("control", "treatment"),
                  rt_col = RT,
                  batch_column = Batch,
                  batch = 1)

  expect_equal(colnames(tbl_rt_non_adj), c("m.z", "p.value", "t.score", "r.t"))
  expect_equal(colnames(tbl_rt_adj), c("m.z", "p.value", "t.score", "r.t"))
  expect_equal(colnames(tbl_rt_filtered), c("m.z", "p.value", "t.score", "r.t"))
  expect_equal(colnames(tbl_non_rt_filtered), c("m.z", "p.value", "t.score"))

  expect_equal(nrow(tbl_rt_non_adj), length(unique(dplyr::pull(toy_metaboscape, UID))))
  expect_equal(nrow(tbl_rt_adj), length(unique(dplyr::pull(toy_metaboscape, UID))))
  expect_equal(nrow(tbl_rt_filtered), length(unique(dplyr::pull(toy_metaboscape, UID))))
  expect_equal(nrow(tbl_non_rt_filtered), length(unique(dplyr::pull(toy_metaboscape, UID))))
  #tbl_non_adj
  #tbl_non_adj
})
