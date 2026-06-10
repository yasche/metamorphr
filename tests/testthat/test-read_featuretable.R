test_that("read_featuretable() reads a feature table from a string and transforms it to the expected format", {
  # this is the original "feature table":
  #
  # tibble::tribble(
  # ~some_metadata1, ~some_metadata2, ~some_label, ~s1, ~s2, ~s3,
  # "a", "c", "f1", 1, 2, 3,
  # "b", "d", "f2", 4, 5, 6
  # )


  expect_equal(
    read_featuretable(I('"some_metadata1","some_metadata2","some_label","s1","s2","s3"\na,c,f1,1,2,3\nb,d,f2,4,5,6'), label_col = 3, metadata_cols = 1:2),
    test_read_featuretable
  )
})

test_that("read_featuretable() reads a feature table from a csv file and transforms it to the expected format", {
  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = 3, metadata_cols = 1:2),
    test_read_featuretable
  )
})

test_that("read_featuretable() reads a feature table from a tsv file and transforms it to the expected format", {
  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable.tsv"), delim = "\t", label_col = 3, metadata_cols = 1:2),
    test_read_featuretable
  )
})

test_that("read_featuretable() can handle character values for label_col and metadata_cols arguments, 1", {
  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = "some_label", metadata_cols = c("some_metadata1", "some_metadata2")),
    test_read_featuretable
  )
})


test_that("read_featuretable() can handle mixed type values for label_col and metadata_cols arguments", {
  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = 3, metadata_cols = c("some_metadata1", "some_metadata2")),
    test_read_featuretable
  )
})

test_that("read_featuretable() can read feature tables without metadata", {
  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable_no_metadata.csv"), label_col = 1),
    dplyr::select(test_read_featuretable, -c(5, 6))
  )
})

test_that("read_featuretable() throws an error if length(label_col) > 1", {
  expect_error(read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = 1:2))
})

test_that("read_featuretable() throws an error if length(label_col) > 1", {
  expect_error(read_featuretable(test_path("data", "test_read_featuretable.csv"), label_col = c("a", "b")))
})

test_that("`remove_empty_cols` argument works as expected", {
  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable_empty_cols.csv"),
                      label_col = 3,
                      metadata_cols = c(1:2),
                      remove_empty_cols = T,
                      show_removed_cols = F),
    test_read_featuretable
  )

  expect_message(
    read_featuretable(test_path("data", "test_read_featuretable_empty_cols.csv"),
                      label_col = 3,
                      metadata_cols = c(1:2),
                      remove_empty_cols = T,
                      show_removed_cols = T),
    "The following columns were removed: `empty_col`, `more_empty_col`.")

  expect_equal(
    read_featuretable(test_path("data", "test_read_featuretable_empty_cols.csv"),
                      label_col = 3,
                      metadata_cols = c(1:2, 7:8),
                      remove_empty_cols = F,
                      show_removed_cols = F),
    dplyr::bind_cols(test_read_featuretable,
                     tibble::tibble(
                       empty_col = rep(NA, 6),
                       more_empty_col = rep(NA, 6)
                     ))
  )

  # doesn't drop label col even if it is empty
  expect_true(
    all(is.na(read_featuretable(test_path("data", "test_read_featuretable_empty_cols.csv"),
                      label_col = 7,
                      metadata_cols = c(1:3),
                      remove_empty_cols = T,
                      show_removed_cols = F)$Feature))
  )

  expect_message(
    read_featuretable(test_path("data", "test_read_featuretable_empty_cols.csv"),
                      label_col = 7,
                      metadata_cols = c(1:3),
                      remove_empty_cols = T,
                      show_removed_cols = T),
    "The following column was removed: `more_empty_col`.")
})

test_that("Metadata columns are handled correctly if supplied as numerics & if they are empty; fix bug introduced in version 0.4.0", {
  expect_samples <- paste0("S", 1:3)

  expect_message(read_featuretable(I("metadata1,label,metadata2,metadata3,S1,S2,S3
  ABC,DEF,,,1,2,3"), label_col = 2, metadata_cols = 1:3, remove_empty_cols = TRUE), "The following columns were removed: `metadata2`, `metadata3`.")
  expect_equal(read_featuretable(I("metadata1,label,metadata2,metadata3,S1,S2,S3
  ABC,DEF,,,1,2,3"), label_col = 2, metadata_cols = 1:3, remove_empty_cols = TRUE, show_removed_cols = FALSE)$Sample,
               expect_samples)
})
