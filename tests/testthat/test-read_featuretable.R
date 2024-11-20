test_that("read_featuretable() reads a feature table from a string and transforms it to the expected format", {
  #this is the original "feature table":
  #
  #tibble::tribble(
  #~some_metadata1, ~some_metadata2, ~some_label, ~s1, ~s2, ~s3,
  #"a", "c", "f1", 1, 2, 3,
  #"b", "d", "f2", 4, 5, 6
  #)


  expect_equal(read_featuretable('"some_metadata1","some_metadata2","some_label","s1","s2","s3"\na,c,f1,1,2,3\nb,d,f2,4,5,6', label_col = 3, metadata_cols = 1:2),
               test_read_featuretable)
})

test_that("read_featuretable() reads a feature table from a csv file and transforms it to the expected format", {

  expect_equal(read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = 3, metadata_cols = 1:2),
               test_read_featuretable)
})

test_that("read_featuretable() can handle character values for label_col and metadata_cols arguments", {

  expect_equal(read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = "some_label", metadata_cols = c("some_metadata1", "some_metadata2")),
               test_read_featuretable)
})


test_that("read_featuretable() can handle mixed type values for label_col and metadata_cols arguments", {

  expect_equal(read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = 3, metadata_cols = c("some_metadata1", "some_metadata2")),
               test_read_featuretable)
})

test_that("read_featuretable() can read feature tables without metadata", {

  expect_equal(read_featuretable(test_path("data", "test_read_featuretable_no_metadata.csv"), label_col = 1),
               dplyr::select(test_read_featuretable, -c(5, 6)))
})
