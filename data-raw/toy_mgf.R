## code to prepare `toy_mgf` dataset goes here

devtools::load_all()

toy_mgf <- metamorphr::read_mgf("inst/extdata/toy_mgf.mgf")

usethis::use_data(toy_mgf, overwrite = TRUE)
