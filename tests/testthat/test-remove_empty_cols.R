test_that("`remove_empty_cols` removes correct columns", {
  test_tibble1 <- tibble::tibble(a = c(1, 2, 3), b = c(NA, 2, 3), c = c(NA, NA, 3), d = c(NA, NA, NA))
  test_tibble2 <- tibble::tibble(a = c(1, 2, 3), b = c(NA, NA, NA), c = c(NA, NA, 3), d = c(NA, NA, NA))
  test_tibble3 <- tibble::tibble(a = c(1, 2, 3), b = c(1, 2, 3), c = c(1, 2, 3), d = c(1, 2, 3))

  test_tibble4 <- tibble::tibble(a = c("a", "b", "c"), b = c(NA, "b", "c"), c = c(NA, NA, "c"), d = c(NA, NA, NA))
  test_tibble5 <- tibble::tibble(a = c("a", "b", "c"), b = c(NA, NA, NA), c = c(NA, NA, "c"), d = c(NA, NA, NA))
  test_tibble6 <- tibble::tibble(a = c("a", "b", "c"), b = c("a", "b", "c"), c = c("a", "b", "c"), d = c("a", "b", "c"))

  test_tibble_nonsynt <- tibble::tibble(`123` = c("a", "b", "c"), `_ABC` = c(NA, "b", "c"), `if` = c(NA, NA, NA), `<-` = c(NA, NA, NA))

  expect_equal(
    remove_empty_cols(test_tibble1, show_removed_cols = FALSE),
    dplyr::select(test_tibble1, -d)
  )
  expect_equal(
    remove_empty_cols(test_tibble2, show_removed_cols = FALSE),
    dplyr::select(test_tibble2, -b, -d)
  )
  expect_equal(
    remove_empty_cols(test_tibble3, show_removed_cols = FALSE),
    test_tibble3
  )

  expect_equal(
    remove_empty_cols(test_tibble4, show_removed_cols = FALSE),
    dplyr::select(test_tibble4, -d)
  )
  expect_equal(
    remove_empty_cols(test_tibble5, show_removed_cols = FALSE),
    dplyr::select(test_tibble5, -b, -d)
  )
  expect_equal(
    remove_empty_cols(test_tibble6, show_removed_cols = FALSE),
    test_tibble6
  )

  expect_equal(
    remove_empty_cols(test_tibble_nonsynt, show_removed_cols = FALSE),
    dplyr::select(test_tibble_nonsynt, -c(3, 4))
  )
})

test_that("message is as expected", {
  test_tibble1 <- tibble::tibble(a = c(1, 2, 3), b = c(NA, 2, 3), c = c(NA, NA, 3), d = c(NA, NA, NA))
  test_tibble2 <- tibble::tibble(a = c(1, 2, 3), b = c(NA, NA, NA), c = c(NA, NA, 3), d = c(NA, NA, NA))
  test_tibble3 <- tibble::tibble(a = c(1, 2, 3), b = c(1, 2, 3), c = c(1, 2, 3), d = c(1, 2, 3))

  test_tibble4 <- tibble::tibble(a = c("a", "b", "c"), b = c(NA, "b", "c"), c = c(NA, NA, "c"), d = c(NA, NA, NA))
  test_tibble5 <- tibble::tibble(a = c("a", "b", "c"), b = c(NA, NA, NA), c = c(NA, NA, "c"), d = c(NA, NA, NA))
  test_tibble6 <- tibble::tibble(a = c("a", "b", "c"), b = c("a", "b", "c"), c = c("a", "b", "c"), d = c("a", "b", "c"))

  test_tibble_nonsynt <- tibble::tibble(`123` = c("a", "b", "c"), `_ABC` = c(NA, "b", "c"), `if` = c(NA, NA, NA), `<-` = c(NA, NA, NA))

  expect_message(
    remove_empty_cols(test_tibble1, show_removed_cols = TRUE),
    "The following column was removed: `d`."
  )
  expect_message(
    remove_empty_cols(test_tibble2, show_removed_cols = TRUE),
    "The following columns were removed: `b`, `d`."
  )
  expect_message(
    remove_empty_cols(test_tibble3, show_removed_cols = TRUE),
    NULL
  )

  expect_message(
    remove_empty_cols(test_tibble4, show_removed_cols = TRUE),
    "The following column was removed: `d`."
  )
  expect_message(
    remove_empty_cols(test_tibble5, show_removed_cols = TRUE),
    "The following columns were removed: `b`, `d`."
  )
  expect_message(
    remove_empty_cols(test_tibble6, show_removed_cols = TRUE),
    NULL
  )

  expect_message(
    remove_empty_cols(test_tibble_nonsynt, show_removed_cols = TRUE),
    "The following columns were removed: `if`, `<-`."
  )
})
