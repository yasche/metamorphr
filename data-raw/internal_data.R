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

test_filters <- metamorphr::read_featuretable(
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

## code to prepare `test_mv_filters_metadata1` dataset goes here
#version 1:
#s1 - s6:   g1
#s7 - s12:  g2
test_filters_metadata1 <- metamorphr::read_featuretable(
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
) %>%
  create_metadata_skeleton() %>%
  dplyr::mutate(Group = c(rep("g1", 6), rep("g2", 6), rep("q", 3), "b")) %>%
  dplyr::mutate(Replicate = c(rep(1:6, 2), 1:3, 1L)) %>%
  dplyr::mutate(Batch = 1L)

## code to prepare `test_mv_filters_metadata2` dataset goes here
#version 1:
#s1 - s3:   g1
#s4 - s6:   g2
#s7 - s9:   g3
#s10-s12:   g4
test_filters_metadata2 <- metamorphr::read_featuretable(
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
) %>%
  create_metadata_skeleton() %>%
  dplyr::mutate(Group = c(rep("g1", 3), rep("g2", 3), rep("g3", 3), rep("g4", 3), rep("q", 3), "b")) %>%
  dplyr::mutate(Replicate = c(rep(1:3, 4), 1:3, 1L)) %>%
  dplyr::mutate(Batch = 1L)

usethis::use_data(test_read_featuretable, test_create_metadata_skeleton, test_filters, test_filters_metadata1, test_filters_metadata2, overwrite = TRUE, internal = TRUE)
