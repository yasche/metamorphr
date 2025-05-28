test_that("correct number of samples", {
  expect_output(summary_featuretable(toy_metaboscape), "11 Samples")
})

test_that("correct number of features", {
  expect_output(summary_featuretable(toy_metaboscape), "10 Features")
})

test_that("number of NA and percentage and total number of observations are correct", {
  calced_na <- toy_metaboscape %>%
    summary_featuretable() %>%
    purrr::map(`[[`, 7) %>%
    unlist() %>%
    sum()

  calced_tot <- nrow(toy_metaboscape)

  expect_output(summary_featuretable(toy_metaboscape), paste0(as.character(round((calced_na / calced_tot) * 100)), " % missing values"))
  expect_output(summary_featuretable(toy_metaboscape), paste0(as.character(calced_na), " out of"))
  expect_output(summary_featuretable(toy_metaboscape), paste0(" out of ", as.character(calced_tot)))
})

test_that("correct number of groups after joining metadata", {
  expect_output(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% summary_featuretable(), "4 Groups")
})

test_that("correct number of replicates after joining metadata", {
  expect_output(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% summary_featuretable(), "Replicates detected: 1...3")
})

test_that("correct number of batches after joining metadata", {
  expect_output(toy_metaboscape %>% join_metadata(toy_metaboscape_metadata) %>% summary_featuretable(), "1 Batches: 1")
})
