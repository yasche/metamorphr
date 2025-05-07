#' Draws a Volcano Plot or performs calculations necessary to draw one manually
#'
#' @description
#' Performs necessary calculations (i.e., calculate <i>p</i>-values and log<sub>2</sub>-fold changes) and creates a basic <a href = "https://en.wikipedia.org/wiki/Volcano_plot_(statistics)">Volcano Plot</a>.
#' The plot is drawn with <a href = "https://ggplot2.tidyverse.org/">ggplot2</a> and can therefore be easily manipulated afterwards (e.g., changing the theme or the axis labels).
#' Please note that the function is intended to be easy to use and beginner friendly and therefore offers limited ability to fine-tune certain parameters of the resulting plot.
#' If you wish to draw the plot yourself, you can set `return_tbl = TRUE`. In this case, a tibble is returned instead of a ggplot2 object which you can use to create a plot yourself.
#' A Volcano Plot is used to compare two groups. Therefore grouping information must be provided. See \code{\link[metamorphr]{join_metadata}} for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param name_column Which column contains the feature names? Can for example be `name_column = UID` or `name_column = Feature`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param groups_to_compare Names of the groups which should be compared as a character vector. Those are the group names in the `group_column`. They are usually provided in the form of a metadata tibble and joined via \code{\link[metamorphr]{join_metadata}}.
#' @param log2fc_cutoff A numeric. What cutoff should be used for the log<sub>2</sub> fold change? Traditionally, this is set to `1` which corresponds to a doubling or halving of intensity or area compared to a control. This is only important for assignment to groups and colors defined in the `colors` argument.
#' @param p_value_cutoff A numeric. What cutoff should be used for the <i>p</i>-value? Traditionally, this is set to `0.05`. This is only important for assignment to groups and colors defined in the `colors` argument. Note that this is not the -log<sub>10</sub> transformed value.
#' @param colors A named list for coloring the dots in the Volcano Plot or `NULL` in case the points should not be colored. The list must contain colors for the following groups: `sig_up`, `sig_down`, `not_sig_up`, `not_sig_down` and `not_sig`.
#' @param adjust_p Should the <i>p</i>-value be adjusted? Can be either `FALSE`, (the default) in case no adjustment should be made or any or the name from \code{\link[stats]{p.adjust.methods}} (e.g., `adjust_p = "fdr"`).
#' @param log2_before A logical. Should the data be log<sub>2</sub> transformed prior to calculating the <i>p</i>-values?
#' @param return_tbl A logical. If `FALSE`, returns a ggplot2 object, if `TRUE` returns a tibble which can be used to draw the plot manually to have more control.
#' @param ... Arguments passed on to \code{\link[stats]{t.test}}. If none are provided (the default), a Welch Two Sample <i>t</i>-test will be performed.
#'
#' @return Either a Volcano Plot in the form of a ggplot2 object or a tibble.
#' @export
#'
#' @examples
#' #returns a Volcano Plot in the form of a ggplot2 object
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   plot_volcano(group_column = Group,
#'                name_column = Feature,
#'                groups_to_compare = c("control", "treatment"))
#'
#' #returns a tibble to draw the plot manually
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   plot_volcano(group_column = Group,
#'                name_column = Feature,
#'                groups_to_compare = c("control", "treatment"),
#'                return_tbl = TRUE)
plot_volcano <- function(data, group_column, name_column, groups_to_compare, log2fc_cutoff = 1, p_value_cutoff = 0.05, colors = list(sig_up = "darkred", sig_down = "darkblue", not_sig_up = "grey", not_sig_down = "grey", not_sig = "grey"), adjust_p = FALSE, log2_before = FALSE, return_tbl = FALSE, ...) {
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
    dplyr::mutate(log2fc = purrr::map(.data$data, internal_l2fc, group_column = {{ group_column }}, groups_to_compare = groups_to_compare, log2_before)) %>%
    dplyr::mutate(p_val = purrr::map(.data$t_test, pull_safely, .data$p.value))

  if (is.na(adjust_p_lgl)) {
    if (!(adjust_p %in% stats::p.adjust.methods)) {
      stop(paste("The method you have provided for adjust_p does not exist!\nPlease use one of the available methods below or set adjust_p = FALSE\nAvailable methods:",
                 paste(stats::p.adjust.methods, collapse = ", ")))
    }

    data$p_val <- stats::p.adjust(data$p_val, method = adjust_p)
  }

  data <- data %>%
    dplyr::mutate(log2fc = as.numeric(.data$log2fc)) %>%
    dplyr::mutate(log2fc = dplyr::case_when(is.nan(.data$log2fc) ~ NA,
                                          .default = .data$log2fc)) %>%
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
        dplyr::mutate(group = dplyr::case_when(.data$p_val <= .env$p_value_cutoff & .data$log2fc <= -.env$log2fc_cutoff ~ "sig_down",
                                               .data$p_val <= .env$p_value_cutoff & .data$log2fc >= .env$log2fc_cutoff ~ "sig_up",
                                               .data$p_val > .env$p_value_cutoff & .data$log2fc <= -.env$log2fc_cutoff ~ "not_sig_down",
                                               .data$p_val > .env$p_value_cutoff & .data$log2fc >= .env$log2fc_cutoff ~ "not_sig_up",
                                               .default = "not_sig")) %>%
        dplyr::left_join(colors, by = "group")

      p <- ggplot2::ggplot(data, ggplot2::aes(.data$log2fc, .data$n_log_p_val)) +
        ggplot2::geom_point(color = data$cols)
    } else {
      p <- ggplot2::ggplot(data, ggplot2::aes(.data$log2fc, .data$n_log_p_val)) +
        ggplot2::geom_point()
    }


  }
  p <- p +
    ggplot2::xlab(bquote(log[2]*(.(groups_to_compare[[2]]))~-~log[2]*(.(groups_to_compare[[1]])))) +
    ggplot2::ylab(bquote(-log[10](italic(p))))

  p
}
