## code to prepare `toy_metaboscape` dataset goes here

devtools::load_all()

toy_metaboscape <- metamorphr::read_featuretable("inst/extdata/toy_metaboscape.csv", metadata_cols = 2:5)

usethis::use_data(toy_metaboscape, overwrite = TRUE)
