test_that("reference `label_col` by name and index are equal", {
  featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")

  ft_1 <- read_featuretable_mzmine(featuretable_path, label_col = 1)
  ft_2 <- read_featuretable_mzmine(featuretable_path, label_col = "id")

  expect_equal(ft_1, ft_2)
})

test_that("Values for `intensity` other than 'area' and 'height' throw an error.", {
  featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")

  expect_error(read_featuretable_mzmine(featuretable_path, label_col = 1, intensity = "abc"), 'Argument `intensity` must be "height" or "area", not "abc".')
})

test_that("Import works as expected for `intensity = 'area' and 'height'`", {
  featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")

  ft_area <- read_featuretable_mzmine(featuretable_path, label_col = "mz", intensity = "area")
  ft_height <- read_featuretable_mzmine(featuretable_path, label_col = "mz", intensity = "height")

  ft_area_res1 <- dplyr::left_join(dplyr::select(ft_area, id, Sample, Intensity), test_read_mzmine_area, by = c("id", "Sample"))
  ft_height_res1 <- dplyr::left_join(dplyr::select(ft_height, id, Sample, Intensity), test_read_mzmine_height, by = c("id", "Sample"))

  expect_true(all(dplyr::near(ft_area_res1$Intensity, ft_area_res1$Intensityx)))
  expect_true(all(dplyr::near(ft_height_res1$Intensity, ft_height_res1$Intensityx)))
})


test_that("datafile columns are present if `import_datafile_cols = TRUE`", {
  featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")

  ft_area <- read_featuretable_mzmine(featuretable_path, import_datafile_cols = TRUE)

  expected_colnames <- readr::read_lines("datafile:feature_state
datafile:mz
datafile:mz_range:min
datafile:mz_range:max
datafile:rt
datafile:rt_range:min
datafile:rt_range:max
datafile:fwhm
datafile:ion_mobility_range:min
datafile:ion_mobility_range:max
datafile:ccs
datafile:ion_mobility_unit
datafile:area
datafile:height
datafile:intensity_range:min
datafile:intensity_range:max
datafile:charge
datafile:fragment_scans
datafile:isotopes
datafile:tailing_factor
datafile:asymmetry_factor
datafile:ion_mobility
datafile:msms_info
datafile:rt_ms2_apex_distance")

  expect_true(all(unlist(purrr::map(expected_colnames, `%in%`, colnames(ft_area)))))
  expect_true(rlang::is_empty(setdiff(expected_colnames, colnames(ft_area))))
})

test_that("datafile columns contain correct information", {
  featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")

  ft_area <- read_featuretable_mzmine(featuretable_path, import_datafile_cols = TRUE, label_col = "mz") %>%
    dplyr::select(id, Sample, dplyr::contains("datafile:"))

  expect_equal(ft_area, test_read_mzmine_datafile)
})
