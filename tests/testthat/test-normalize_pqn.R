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
  # prepare data
  data("MetRef", package = "KODAMA")
  MetRef_data <- MetRef$data
  MetRef_data <- MetRef_data[, -which(colSums(MetRef_data) == 0)]

  MetRef_data <- MetRef_data %>%
    t() %>%
    tibble::as_tibble()

  MetRef_data$UID <- 1:nrow(MetRef_data)

  MetRef_data <- tidyr::gather(MetRef_data, key = "Sample", value = "Intensity", -UID)

  MetRef_data_imputed <- MetRef_data %>%
    dplyr::mutate(Intensity = dplyr::case_when(Intensity == 0 ~ NA,
      .default = Intensity
    )) %>%
    impute_lod()


  MetRef_data_imputed_mat <- MetRef_data_imputed %>%
    tidyr::spread(key = "Sample", value = "Intensity") %>%
    dplyr::select(-UID) %>%
    dplyr::relocate(as.character(1:873)) %>%
    as.matrix() %>%
    t()

  kod_norm <- KODAMA::normalization(MetRef_data_imputed_mat)$newXtrain
  # for some reason, KODAMA::normalization() works with absolute Intensities
  # While this might be sensible for NMR data, I don't see the point for LC-MS data:
  # negative Intensities should not exist, at least not prior to log-transformation.
  mm_norm <- MetRef_data_imputed %>%
    dplyr::group_by(Sample) %>%
    dplyr::mutate(Intensity = Intensity / sum(abs(Intensity))) %>%
    dplyr::ungroup() %>%
    normalize_pqn(normalize_sum = FALSE)

  expect_equal(as.numeric(t(kod_norm)), mm_norm$Intensity)
})

test_that("throws error if method does not exist", {
  expect_error(toy_metaboscape %>%
    join_metadata(toy_metaboscape_metadata) %>%
    impute_lod() %>%
    normalize_pqn(fn = "meanxxx"))
})
