test_that("ref_as_group = TRUE and FALSE give same results; median", {

  rag_false <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(reference_samples = c("QC1", "QC2", "QC3"))

  rag_true <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(reference_samples = c("QC"), ref_as_group = TRUE, group_column = Group)

  expect_equal(rag_false, rag_true)
})


test_that("ref_as_group = TRUE and FALSE give same results; mean", {

  rag_false <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(reference_samples = c("QC1", "QC2", "QC3"), fn = "mean")

  rag_true <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(reference_samples = c("QC"), ref_as_group = TRUE, group_column = Group, fn = "mean")

  expect_equal(rag_false, rag_true)
})
