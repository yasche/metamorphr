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


test_that("ref_as_group = TRUE and FALSE give same results; median", {
  rag_false <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn()

  rag_true <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(ref_as_group = TRUE, group_column = Group)

  expect_equal(rag_false, rag_true)
})


test_that("ref_as_group = TRUE and FALSE give same results; mean", {
  rag_false <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(fn = "mean")

  rag_true <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(ref_as_group = TRUE, group_column = Group, fn = "mean")

  expect_equal(rag_false, rag_true)
})



test_that('result is equivalent to KODAMA::normalization(method = "pqn")', {
  test_rnd_mat_loc <- test_rnd_mat %>%
    t() %>%
    tibble::as_tibble(.name_repair = "universal_quiet")

  test_rnd_mat_loc$UID <- 1:nrow(test_rnd_mat_loc)



  test_rnd_mat_loc <- tidyr::gather(test_rnd_mat_loc, key = "Sample", value = "Intensity", -UID)

  test_rnd_mat_loc <- test_rnd_mat_loc %>%
    dplyr::mutate(Intensity = dplyr::case_when(Intensity == 0 ~ NA,
      .default = Intensity
    ))


  #kod_norm <- KODAMA::normalization(MetRef_data_imputed_mat)$newXtrain
  # for some reason, KODAMA::normalization() works with absolute Intensities
  # While this might be sensible for NMR data, I don't see the point for LC-MS data:
  # negative Intensities should not exist, at least not prior to log-transformation.
  mm_norm_man <- test_rnd_mat_loc %>%
    dplyr::group_by(Sample) %>%
    # Scale to abs sums is necessary if data contains negative values (e.g., in NMR data?)
    dplyr::mutate(Intensity = .data$Intensity / sum(abs(.data$Intensity))) %>%
    dplyr::ungroup() %>%
    normalize_pqn(normalize_sum = FALSE)


  mm_norm_auto <- test_rnd_mat_loc %>%
    normalize_pqn(normalize_sum = TRUE)

  expect_equal(mm_norm_man$Intensity, as.numeric(t(test_rnd_mat_kod_norm)))
  expect_equal(mm_norm_auto$Intensity, as.numeric(t(test_rnd_mat_kod_norm)))
})

test_that("throws error if method does not exist", {
  expect_error(toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(fn = "meanxxx"))
})


test_that("row & column order stays unchanged", {
  joined_df <- toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata)

  normalized_df <- joined_df %>%
    impute_lod() %>%
    normalize_pqn(reference_samples = c("QC1", "QC2", "QC3"))

  normalized_df_rag <- joined_df %>%
    impute_lod() %>%
    normalize_pqn(reference_samples = c("QC"), ref_as_group = TRUE, group_column = Group)

  expect_equal(dplyr::select(normalized_df, -Intensity), dplyr::select(joined_df, -Intensity))
  expect_equal(dplyr::select(normalized_df_rag, -Intensity), dplyr::select(joined_df, -Intensity))
})
