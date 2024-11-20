## code to prepare `test_create_metadata_skeleton` dataset goes here

test_create_metadata_skeleton <- tibble::tribble(
  ~Sample, ~Group, ~Replicate, ~Batch, ~Factor,
  "s1", NA, NA, NA, 1,
  "s2", NA, NA, NA, 1,
  "s3", NA, NA, NA, 1,
)

usethis::use_data(test_create_metadata_skeleton, overwrite = TRUE, internal = TRUE)
