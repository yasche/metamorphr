test_that("normalize_ref works for UID = 2", {
  toy_metaboscape_normalized <- toy_metaboscape %>%
    normalize_ref(reference_feature = 2, identifier_column = UID)

  expect_equal(toy_metaboscape_normalized, test_normalize_ref_uid_2)
})

test_that("normalize_ref works for UID = 2 and reference_feature_intensity = mean", {
  toy_metaboscape_normalized <- toy_metaboscape %>%
    normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = mean)

  expect_equal(toy_metaboscape_normalized, test_normalize_ref_uid_2_mul_mean)
})

test_that("normalize_ref works for UID = 2 and reference_feature_intensity = median", {
  toy_metaboscape_normalized <- toy_metaboscape %>%
    normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = median)

  expect_equal(toy_metaboscape_normalized, test_normalize_ref_uid_2_mul_med)
})

test_that("normalize_ref works for Name = 'NADPH'", {
  toy_metaboscape_normalized <- toy_metaboscape %>%
    dplyr::filter(Sample != "Blank1" & Sample != "Blank2") %>%
    normalize_ref(reference_feature = "ADP", identifier_column = Name)

  expect_equal(toy_metaboscape_normalized, test_normalize_ref_name_adp)
})


test_that("normalize_ref throws error if some ref feature intensity is NA", {
  expect_error(normalize_ref(toy_metaboscape, reference_feature = "NADPH", identifier_column = Name))
})

test_that("normalize_ref throws error if some ref feature does not exist", {
  expect_error(normalize_ref(toy_metaboscape, reference_feature = "NADPHxxxx", identifier_column = Name))
})

test_that("normalize_ref throws error if reference feature exists multiple times", {
  toy_metaboscape_edit <- toy_metaboscape %>%
    dplyr::mutate(Name = dplyr::case_when(
      Formula == "C7H15NO3" ~ "Multiple",
      Formula == "C5H14NO" ~ "Multiple"
    ))

  expect_error(normalize_ref(toy_metaboscape_edit, reference_feature = "Multiple", identifier_column = Name))
})


test_that("row & column order stays unchanged", {
  normalized_df <- toy_metaboscape %>%
    impute_lod() %>%
    normalize_ref(reference_feature = 2, identifier_column = UID)

  expect_equal(dplyr::select(normalized_df, -Intensity), dplyr::select(toy_metaboscape, -Intensity))
})
