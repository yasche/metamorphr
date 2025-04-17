## code to prepare `toy_metaboscape_metadata` dataset goes here

devtools::load_all()

toy_metaboscape_metadata <- toy_metaboscape %>%
  create_metadata_skeleton() %>%
  dplyr::mutate(Group = c(rep("control", 3), rep("treatment", 3), rep("QC", 3), rep("blank", 2))) %>%
  dplyr::mutate(Replicate = c(rep(1:3, 3), 1:2)) %>%
  dplyr::mutate(Batch = 1L) %>%
  dplyr::mutate(Factor = rnorm(n = nrow(.), mean = 1, sd = 0.2))

usethis::use_data(toy_metaboscape_metadata, overwrite = TRUE)
