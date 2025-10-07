test_that("Parsing and replacement works for all standard element symbols", {
  all_atoms_str <- paste(c(names(other_atoms_multi_lookup), names(other_atoms_single_lookup)), collapse = "")
  ftm_result <- formula_to_mass(all_atoms_str)

  manual_result <- c(unname(other_atoms_multi_lookup), unname(other_atoms_single_lookup)) %>%
    stringr::str_remove_all("\\+") %>%
    stringr::str_remove_all("\\*") %>%
    as.numeric() %>%
    sum()

  expect_equal(ftm_result, manual_result)
})

test_that("Parsing and replacement works for all special element symbols", {
  all_atoms_str <- paste(names(special_isos_lookup), collapse = "")
  ftm_result <- formula_to_mass(all_atoms_str)

  manual_result <- unname(special_isos_lookup) %>%
    stringr::str_remove_all("\\+") %>%
    stringr::str_remove_all("\\*") %>%
    as.numeric() %>%
    sum()

  expect_equal(ftm_result, manual_result)
})

test_that("Parsing and replacement works for all standard element symbols and calculated mass matches reference", {
  # reference mass is calculated using https://bmrb.io/metabolomics/mol_mass.php
  # with the following string:
  # HeLiBeNeNaMgAlSiClArCaScTiCrMnFeCoNiCuZnGaGeAsSeBrKrRbSrZrNbMoRuRhPdAgCdInSnSbTeXeCsBaLaCePrNdSmEuGdTbDyHoErTmYbLuHfTaReOsIrPtAuHgTlPbBiThHBCNOFPSKVYIWU
  # Pa, D and T must be removed
  all_atoms_str <- c(names(other_atoms_multi_lookup), names(other_atoms_single_lookup))
  all_atoms_str <- all_atoms_str[!(all_atoms_str %in% c("D", "T", "Pa"))]
  all_atoms_str <- paste(all_atoms_str, collapse = "")
  ftm_result <- formula_to_mass(all_atoms_str)

  # round due to slight variation in exact mass
  expect_equal(round(ftm_result, digits = 3), round(8530.4427021607, digits = 3))
  #expect_equal(round(ftm_result, digits = 3), round(8530.443011, digits = 3))
})

test_that("Brackets are resolved and calculated mass matches reference for examples", {
  # reference mass is calculated using https://bmrb.io/metabolomics/mol_mass.php
  # with the following strings:
  # C(((H3)(N4))3)5
  # K2P7(((N4)(O5))2((S6)(Cl3))3)7
  # Li5((((Br3)2)H5)6)6

  m1 <- formula_to_mass("C(((H3)(N4))3)5")
  m2 <- formula_to_mass("K2P7(((N4)(O5))2((S6)(Cl3))3)7")
  m3 <- formula_to_mass("Li5((((Br3)2)H5)6)6")

  expect_equal(m1, 897.5365667565)
  expect_equal(m2, 8430.0785398782)
  expect_equal(m3, 17262.8494473780)

})


test_that("Calculated mass matches reference mass for 5 random molecules that contain only CHNOPS", {
  # reference mass is calculated using https://bmrb.io/metabolomics/mol_mass.php

  set.seed(123)
  rnd_chnops1 <- paste0(paste0("C", as.character(sample(1:100, 1))),
                        paste0("H", as.character(sample(1:100, 1))),
                        paste0("N", as.character(sample(1:100, 1))),
                        paste0("O", as.character(sample(1:100, 1))),
                        paste0("P", as.character(sample(1:100, 1))),
                        paste0("S", as.character(sample(1:100, 1))))

  set.seed(234)
  rnd_chnops2 <- paste0(paste0("C", as.character(sample(1:100, 1))),
                        paste0("H", as.character(sample(1:100, 1))),
                        paste0("N", as.character(sample(1:100, 1))),
                        paste0("O", as.character(sample(1:100, 1))),
                        paste0("P", as.character(sample(1:100, 1))),
                        paste0("S", as.character(sample(1:100, 1))))

  set.seed(345)
  rnd_chnops3 <- paste0(paste0("C", as.character(sample(1:100, 1))),
                        paste0("H", as.character(sample(1:100, 1))),
                        paste0("N", as.character(sample(1:100, 1))),
                        paste0("O", as.character(sample(1:100, 1))),
                        paste0("P", as.character(sample(1:100, 1))),
                        paste0("S", as.character(sample(1:100, 1))))

  set.seed(456)
  rnd_chnops4 <- paste0(paste0("C", as.character(sample(1:100, 1))),
                        paste0("H", as.character(sample(1:100, 1))),
                        paste0("N", as.character(sample(1:100, 1))),
                        paste0("O", as.character(sample(1:100, 1))),
                        paste0("P", as.character(sample(1:100, 1))),
                        paste0("S", as.character(sample(1:100, 1))))

  set.seed(567)
  rnd_chnops5 <- paste0(paste0("C", as.character(sample(1:100, 1))),
                        paste0("H", as.character(sample(1:100, 1))),
                        paste0("N", as.character(sample(1:100, 1))),
                        paste0("O", as.character(sample(1:100, 1))),
                        paste0("P", as.character(sample(1:100, 1))),
                        paste0("S", as.character(sample(1:100, 1))))


  expect_equal(formula_to_mass(rnd_chnops1), 4807.7727466605)
  expect_equal(formula_to_mass(rnd_chnops2), 4035.7183285485)
  expect_equal(formula_to_mass(rnd_chnops3), 6134.5053355338)
  expect_equal(formula_to_mass(rnd_chnops4), 5348.5868880285)
  expect_equal(formula_to_mass(rnd_chnops5), 6451.5026485367)
})

test_that("Special isotopes and brackets are resolved and calculated mass matches reference for examples", {
  # reference mass is calculated using https://www.envipat.eawag.ch/index.php
  # with the following strings:
  # [13]C5[15]NC7H9S3   ([13C]5[15N]C7H9S3)
  # (([2]H3[13]C)3N)5   ((([2H]3[13C])3N)5)
  # (CH3)3[15]N         ((CH3)3[15N])
  # note that the input is slightly different to that of `formula_to_mass` (e.g., [13]C -> [13C])

  m1 <- formula_to_mass("[13C]5[15N]C7H9S3")
  m2 <- formula_to_mass("(([2H]3[13C])3N)5")
  m3 <- formula_to_mass("(CH3)3[15N]")

  expect_equal(round(m1, digits = 4), 269.0035)
  expect_equal(round(m2, digits = 4), 355.7003)
  expect_equal(round(m3, digits = 5), 60.07053)
})
