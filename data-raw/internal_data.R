## code to prepare `test_create_metadata_skeleton` dataset goes here

test_create_metadata_skeleton <- tibble::tribble(
  ~Sample, ~Group, ~Replicate, ~Batch, ~Factor,
  "s1", NA, NA, NA, 1,
  "s2", NA, NA, NA, 1,
  "s3", NA, NA, NA, 1,
)


## code to prepare `test_read_featuretable` dataset goes here

test_read_featuretable <- tibble::tribble(
  ~UID, ~Feature, ~Sample, ~Intensity, ~some_metadata1, ~some_metadata2,
  1, "f1", "s1", 1, "a", "c",
  2, "f2", "s1", 4, "b", "d",
  1, "f1", "s2", 2, "a", "c",
  2, "f2", "s2", 5, "b", "d",
  1, "f1", "s3", 3, "a", "c",
  2, "f2", "s3", 6, "b", "d",
) %>%
  dplyr::mutate(UID = as.integer(UID))

usethis::use_data(test_read_featuretable, test_create_metadata_skeleton, overwrite = TRUE, internal = TRUE)
