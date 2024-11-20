test_that("metadata skeleton for test_read_featuretable is as expected", {
  expect_equal(create_metadata_skeleton(test_read_featuretable),
               test_create_metadata_skeleton)
})
