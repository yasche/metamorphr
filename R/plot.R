plot_volcano <- function(data, group_column, groups_to_compare, adjust_p = FALSE, log2_before = FALSE, ...) {
  if (log2_before == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = log2(.data$Intensity))
  }

  if (length(groups_to_compare) != 2) {
    stop(paste0("A volcano plot can only compare 2 groups! You provided ", length(groups_to_compare), ": ", paste(groups_to_compare, collapse = ", "), "."))
  }

  group_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(group_column))))

  print(group_column_string)

  data %>%
    dplyr::select({{ group_column }}, "Intensity", "UID") %>%
    dplyr::filter({{ group_column }} %in% groups_to_compare) %>%
    #tidyr::gather(key = "Group", value = "Intensity")
    #tidyr::gather(key = {{ group_column }}, value = "Intensity") #%>%
    dplyr::group_by(.data$UID) %>%
    tidyr::nest() %>%
    #dplyr::mutate(data = purrr::map(data, tidyr::gather, key = {{ group_column }}, value = "Intensity"))
    dplyr::mutate(t_test = purrr::map(data, internal_t_test, group_column = {{ group_column }}, groups_to_compare = groups_to_compare, ...)) %>%
    dplyr::mutate(t_test = purrr::map(t_test, glance_safely))
}
