plot_volcano <- function(data, group_column, name_column, groups_to_compare, l2fc_cutoff = 1, p_value_cutoff = 0.05, colors = list(sig_up = "darkred", sig_down = "darkblue", not_sig_up = "grey", not_sig_down = "grey", not_sig = "grey"), adjust_p = FALSE, log2_before = FALSE, return_tbl = FALSE, ...) {
  if (log2_before == TRUE) {
    data <- data %>%
      dplyr::mutate(Intensity = log2(.data$Intensity))
  }

  if (length(groups_to_compare) != 2) {
    stop(paste0("A volcano plot can only compare 2 groups! You provided ", length(groups_to_compare), ": ", paste(groups_to_compare, collapse = ", "), "."))
  }

  adjust_p_lgl <- as.logical(adjust_p)

  data <- data %>%
    dplyr::select("UID", {{ group_column }}, {{ name_column }}, "Intensity") %>%
    dplyr::filter({{ group_column }} %in% groups_to_compare) %>%
    dplyr::group_by(.data$UID, {{ name_column }}) %>%
    tidyr::nest() %>%
    dplyr::mutate(t_test = purrr::map(.data$data, internal_t_test, group_column = {{ group_column }}, groups_to_compare = groups_to_compare, ...)) %>%
    dplyr::mutate(t_test = purrr::map(.data$t_test, glance_safely)) %>%
    dplyr::mutate(l2fc = purrr::map(.data$data, internal_l2fc, group_column = {{ group_column }}, groups_to_compare = groups_to_compare, log2_before)) %>%
    dplyr::mutate(p_val = purrr::map(.data$t_test, pull_safely, .data$p.value))

  if (is.na(adjust_p_lgl)) {
    if (!(adjust_p %in% stats::p.adjust.methods)) {
      stop(paste("The method you have provided for adjust_p does not exist!\nPlease use one of the available methods below or set adjust_p = FALSE\nAvailable methods:",
                 paste(stats::p.adjust.methods, collapse = ", ")))
    }

    data$p_val <- stats::p.adjust(data$p_val, method = adjust_p)
  }

  data <- data %>%
    dplyr::mutate(l2fc = as.numeric(.data$l2fc)) %>%
    dplyr::mutate(l2fc = dplyr::case_when(is.nan(.data$l2fc) ~ NA,
                                          .default = .data$l2fc)) %>%
    dplyr::mutate(p_val = as.numeric(.data$p_val)) %>%
    dplyr::mutate(n_log_p_val = -log10(.data$p_val)) %>%
    dplyr::select(-"data", -"t_test")

  if (return_tbl == TRUE) {
    return(data)
  } else {
    if (!is.null(colors)) {
      colors = tibble::tibble(
        group = names(colors),
        cols = unlist(colors)
      )

      data <- data %>%
        dplyr::mutate(group = dplyr::case_when(.data$p_val <= .env$p_value_cutoff & .data$l2fc <= -.env$l2fc_cutoff ~ "sig_down",
                                               .data$p_val <= .env$p_value_cutoff & .data$l2fc >= .env$l2fc_cutoff ~ "sig_up",
                                               .data$p_val > .env$p_value_cutoff & .data$l2fc <= -.env$l2fc_cutoff ~ "not_sig_down",
                                               .data$p_val > .env$p_value_cutoff & .data$l2fc >= .env$l2fc_cutoff ~ "not_sig_up",
                                               .default = "not_sig")) %>%
        dplyr::left_join(colors, by = "group")

      p <- ggplot2::ggplot(data, ggplot2::aes(.data$l2fc, .data$n_log_p_val)) +
        ggplot2::geom_point(color = data$cols)
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(.data$l2fc, .data$n_log_p_val)) +
        ggplot2::geom_point()
    }


  }
  p <- p +
    ggplot2::xlab(bquote(log[2]*(.(groups_to_compare[[2]]))~-~log[2]*(.(groups_to_compare[[1]])))) +
    ggplot2::ylab(bquote(-log[10](italic(p))))

  p
}
