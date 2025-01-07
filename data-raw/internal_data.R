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
  metamorphr::create_metadata_skeleton() %>%
  dplyr::mutate(Group = c(rep("g1", 3), rep("g2", 3), rep("g3", 3), rep("g4", 3), rep("q", 3), "b")) %>%
  dplyr::mutate(Replicate = c(rep(1:3, 4), 1:3, 1L)) %>%
  dplyr::mutate(Batch = 1L)

## code to prepare `test_qn_data` dataset goes here
# data is from
# Y. Zhao, L. Wong, W. W. B. Goh, Sci Rep 2020, 10, 15534, DOI 10.1038/s41598-020-72664-6.
# A1 - B2 are labeled S1 - S8
test_qn_data <- metamorphr::read_featuretable(
  "label,S1,S2,S3,S4,S5,S6,S7,S8
A,2,3,6,5,7,5,8,8
B,4,3,5,3,6,7,6,7
C,5,4,3,4,9,4,7,7
D,3,5,4,4,7,8,5,6
E,4,5,5,6,8,7,7,9"
)

## code to prepare `test_qn_metadata` dataset goes here
test_qn_metadata <- test_qn_data %>%
  metamorphr::create_metadata_skeleton() %>%
  dplyr::mutate(Group  = c(rep("A", 4), rep("B", 4)),
                Replicate = c(1:4, 1:4),
                Batch = rep(c(1L, 2L), each = 2, times = 2))

## code to prepare `test_qn_all_results` dataset goes here
test_qn_all_results <- metamorphr::read_featuretable(
  "label,S1,S2,S3,S4,S5,S6,S7,S8
A,4,4.44,7,6.13,5.25,4.88,7,6.13
B,5.88,4.44,5.88,4,4,5.88,4.88,5.25
C,7,5.63,4,5.25,7,4,5.88,5.25
D,4.88,6.56,4.88,5.25,5.25,7,4,4
E,5.88,6.56,5.88,7,6.13,5.88,5.88,7"
)



usethis::use_data(test_read_featuretable,
                  test_create_metadata_skeleton,
                  test_filters,
                  test_filters_metadata1,
                  test_filters_metadata2,
                  test_qn_data,
                  test_qn_metadata,
                  test_qn_all_results,
                  overwrite = TRUE, internal = TRUE)
