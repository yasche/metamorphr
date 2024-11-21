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


## code to prepare `test_mv_filters` dataset goes here

test_mv_filters <- metamorphr::read_featuretable(
"label,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,q1,q2,q3,b1
f0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0.1
f1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1.17,0.2
f2,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1.38,0.3
f3,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1.6,0.4
f4,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1.85,0.5
f5,1,1,1,1,1,0,0,0,0,0,0,0,1,1,2.2,0.6
f6,1,1,1,1,1,1,0,0,0,0,0,0,1,1,2.55,0.7
f7,1,1,1,1,1,1,1,0,0,0,0,0,1,1,3,0.8
f8,1,1,1,1,1,1,1,1,0,0,0,0,1,1,3.5,0.9
f9,1,1,1,1,1,1,1,1,1,0,0,0,1,1,4.2,1
f10,1,1,1,1,1,1,1,1,1,1,0,0,1,1,5,0
f11,1,1,1,1,1,1,1,1,1,1,1,0,1,1,6,0.5
f12,1,1,1,1,1,1,1,1,1,1,1,1,1,1,7.5,1"
)



usethis::use_data(test_read_featuretable, test_create_metadata_skeleton, test_mv_filters, overwrite = TRUE, internal = TRUE)
