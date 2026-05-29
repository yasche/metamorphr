convert_from_wide <- function(data, label_col = 1, metadata_cols = NULL) {
  # perform some checks
  if (length(label_col) > 1) {
    rlang::abort("label_col must be of length 1.")
  }

  data_colnames <- colnames(data)

  # functionality for when label_col is a character
  if (is.character(label_col)) {
    label_col <- which(data_colnames == label_col)
  }

  # functionality for when metadata_cols is a character
  if (all(!(is.null(metadata_cols)), is.character(metadata_cols))) {
    metadata_cols <- which(data_colnames %in% metadata_cols)
  }

  # 1: always UID
  metadata_cols <- c(1, metadata_cols + 1, label_col + 1)

  metadata_cols <- unique(metadata_cols)

  # renamed Measurement -> Sample; label -> Feature
  data %>%
    tibble::as_tibble() %>%
    dplyr::rename("Feature" = dplyr::all_of(label_col)) %>%
    # select(- {{ drop_cols }}) %>%
    dplyr::mutate(Feature = as.character(.data$Feature)) %>%
    dplyr::mutate(UID = seq(1, length(.data$Feature))) %>%
    dplyr::relocate("UID", .before = 1) %>%
    dplyr::relocate("Feature", .after = 1) %>%
    # print()
    tidyr::gather(-dplyr::all_of(metadata_cols), key = "Sample", value = "Intensity") %>%
    dplyr::relocate("Sample", .after = 2) %>%
    dplyr::relocate("Intensity", .after = 3) %>%
    dplyr::mutate(Intensity = as.numeric(.data$Intensity)) %>%
    # replace 0 with NA
    dplyr::mutate(Intensity = dplyr::na_if(.data$Intensity, 0))
}
