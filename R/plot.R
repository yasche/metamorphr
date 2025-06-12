#' Draws a Volcano Plot or performs calculations necessary to draw one manually
#'
#' @description
#' Performs necessary calculations (i.e., calculate \emph{p}-values and log2-fold changes) and creates a basic \href{https://en.wikipedia.org/wiki/Volcano_plot_(statistics)}{Volcano Plot}.
#' The plot is drawn with \href{https://ggplot2.tidyverse.org/}{ggplot2} and can therefore be easily manipulated afterwards (e.g., changing the theme or the axis labels).
#' Please note that the function is intended to be easy to use and beginner friendly and therefore offers limited ability to fine-tune certain parameters of the resulting plot.
#' If you wish to draw the plot yourself, you can set `return_tbl = TRUE`. In this case, a tibble is returned instead of a ggplot2 object which you can use to create a plot yourself.
#' A Volcano Plot is used to compare two groups. Therefore grouping information must be provided. See \code{\link[metamorphr]{join_metadata}} for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param name_column Which column contains the feature names? Can for example be `name_column = UID` or `name_column = Feature`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param groups_to_compare Names of the groups which should be compared as a character vector. Those are the group names in the `group_column`. They are usually provided in the form of a metadata tibble and joined via \code{\link[metamorphr]{join_metadata}}.
#' @param batch_column Which column contains the batch information? Usually `grouping_column = Batch`. Only relevant if `data` contains multiple batches. For example, if `data` contains 2 batches and each batch contains measurements of separate controls, `group_column` and `batch` arguments should be provided. Otherwise controls of both batches will be considered when calculating the \emph{p}-value and log2 fold change. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch The names of the batch(es) that should be included when calculating \emph{p}-value and log2 fold change.
#' @param log2fc_cutoff A numeric. What cutoff should be used for the log2 fold change? Traditionally, this is set to `1` which corresponds to a doubling or halving of intensity or area compared to a control. This is only important for assignment to groups and colors defined in the `colors` argument.
#' @param p_value_cutoff A numeric. What cutoff should be used for the \emph{p}-value? Traditionally, this is set to `0.05`. This is only important for assignment to groups and colors defined in the `colors` argument. Note that this is not the -log10 transformed value.
#' @param colors A named list for coloring the dots in the Volcano Plot or `NULL` in case the points should not be colored. The list must contain colors for the following groups: `sig_up`, `sig_down`, `not_sig_up`, `not_sig_down` and `not_sig`.
#' @param adjust_p Should the \emph{p}-value be adjusted? Can be either `FALSE`, (the default) in case no adjustment should be made or any or the name from \code{\link[stats]{p.adjust.methods}} (e.g., `adjust_p = "fdr"`).
#' @param log2_before A logical. Should the data be log2 transformed prior to calculating the \emph{p}-values?
#' @param return_tbl A logical. If `FALSE`, returns a ggplot2 object, if `TRUE` returns a tibble which can be used to draw the plot manually to have more control.
#' @param ... Arguments passed on to \code{\link[stats]{t.test}}. If none are provided (the default), a Welch Two Sample \emph{t}-test will be performed.
#'
#' @return Either a Volcano Plot in the form of a ggplot2 object or a tibble.
#' @export
#'
#' @examples
#' # returns a Volcano Plot in the form of a ggplot2 object
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   plot_volcano(
#'     group_column = Group,
#'     name_column = Feature,
#'     groups_to_compare = c("control", "treatment")
#'   )
#'
#' # returns a tibble to draw the plot manually
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   plot_volcano(
#'     group_column = Group,
#'     name_column = Feature,
#'     groups_to_compare = c("control", "treatment"),
#'     return_tbl = TRUE
#'   )
plot_volcano <- function(data, group_column, name_column, groups_to_compare, batch_column = NULL, batch = NULL, log2fc_cutoff = 1, p_value_cutoff = 0.05, colors = list(sig_up = "darkred", sig_down = "darkblue", not_sig_up = "grey", not_sig_down = "grey", not_sig = "grey"), adjust_p = FALSE, log2_before = FALSE, return_tbl = FALSE, ...) {
  batch_column_string <- gsub("`$", "", gsub("^`", "", rlang::expr_label(substitute(batch_column))))

  if (batch_column_string != "NULL") {
    data <- data %>%
      dplyr::filter({{ batch_column }} %in% batch)
  }

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
      stop(paste(
        "The method you have provided for adjust_p does not exist!\nPlease use one of the available methods below or set adjust_p = FALSE\nAvailable methods:",
        paste(stats::p.adjust.methods, collapse = ", ")
      ))
    }

    data$p_val <- stats::p.adjust(data$p_val, method = adjust_p)
  }

  data <- data %>%
    dplyr::mutate(log2fc = as.numeric(.data$log2fc)) %>%
    dplyr::mutate(log2fc = dplyr::case_when(is.nan(.data$log2fc) ~ NA,
      .default = .data$log2fc
    )) %>%
    dplyr::mutate(p_val = as.numeric(.data$p_val)) %>%
    dplyr::mutate(n_log_p_val = -log10(.data$p_val)) %>%
    dplyr::select(-"data", -"t_test") %>%
    dplyr::ungroup()

  if (return_tbl == TRUE) {
    return(data)
  } else {
    if (!is.null(colors)) {
      colors <- tibble::tibble(
        group = names(colors),
        cols = unlist(colors)
      )

      data <- data %>%
        dplyr::mutate(group = dplyr::case_when(.data$p_val <= .env$p_value_cutoff & .data$log2fc <= -.env$log2fc_cutoff ~ "sig_down",
          .data$p_val <= .env$p_value_cutoff & .data$log2fc >= .env$log2fc_cutoff ~ "sig_up",
          .data$p_val > .env$p_value_cutoff & .data$log2fc <= -.env$log2fc_cutoff ~ "not_sig_down",
          .data$p_val > .env$p_value_cutoff & .data$log2fc >= .env$log2fc_cutoff ~ "not_sig_up",
          .default = "not_sig"
        )) %>%
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

#' Draws a scores or loadings plot or performs calculations necessary to draw them manually
#'
#' @description
#' Performs PCA and creates a Scores or Loadings plot. Basically a wrapper around `pcaMethods::`\code{\link[pcaMethods]{pca}}
#' The plot is drawn with \href{https://ggplot2.tidyverse.org/}{ggplot2} and can therefore be easily manipulated afterwards (e.g., changing the theme or the axis labels).
#' Please note that the function is intended to be easy to use and beginner friendly and therefore offers limited ability to fine-tune certain parameters of the resulting plot.
#' If you wish to draw the plot yourself, you can set `return_tbl = TRUE`. In this case, a tibble is returned instead of a ggplot2 object which you can use to create a plot yourself.
#'
#' \strong{Important Note}
#'
#' `plot_pca()` depends on the `pcaMethods` package from Bioconductor. If `metamorphr` was installed via `install.packages()`, dependencies from Bioconductor were not
#' automatically installed. When `plot_pca()` is called without the `pcaMethods` package installed, you should be asked if you want to install `pak` and `pcaMethods`.
#' If you want to use `plot_pca()` you have to install those. In case you run into trouble with the automatic installation, please install `pcaMethods` manually. See
#' \href{https://www.bioconductor.org/packages/release/bioc/html/pcaMethods.html}{pcaMethods – a Bioconductor package providing PCA methods for incomplete data} for instructions on manual installation.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param method A character specifying one of the available methods ("svd", "nipals", "rnipals", "bpca", "ppca", "svdImpute", "robustPca", "nlpca", "llsImpute", "llsImputeAll"). If the default is used ("svd") an SVD PCA will be done, in case `data` does not contain missing values, or a NIPALS PCA if `data` does contain missing values.
#' @param what Specifies what should be returned. Either `"scores"` or `"loadings"`.
#' @param n_pcs The number of PCs to calculate.
#' @param pcs A vector containing 2 integers that specifies the PCs to plot. Only relevant if `return_tbl = FALSE`. The following condition applies: `max(pcs) <= n_pcs`.
#' @param center Should `data` be mean centered? See \code{\link[pcaMethods]{prep}} for details.
#' @param group_column Either `NULL` or a column in `data` (e.g., `group_column = Group`). If provided, the dots in the scores plot will be colored according to their group. Only relevant if `what = "scores"`.
#' @param name_column Either `NULL` or a column in `data` (e.g., `name_column = Feature`). If provided, feature names are preserved in the resulting tibble. Only relevant if `what = "loadings"` & `return_tbl = TRUE`.
#' @param return_tbl A logical. If `FALSE`, returns a ggplot2 object, if `TRUE` returns a tibble which can be used to draw the plot manually to have more control.
#' @param verbose Should outputs from \code{\link[pcaMethods]{pca}} be printed to the console?
#'
#' @return Either a Scores or Loadings Plot in the form of a ggplot2 object or a tibble.
#' @export
#'
#' @examples
#' # Draw a Scores Plot
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   plot_pca(what = "scores", group_column = Group)
#'
#' # Draw a Loadings Plot
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   plot_pca(what = "loadings", name_column = Feature)
plot_pca <- function(data, method = "svd", what = "scores", n_pcs = 2, pcs = c(1, 2), center = TRUE, group_column = NULL, name_column = NULL, return_tbl = FALSE, verbose = FALSE) {
  # pcaMethods is a bioconductor package so it is not installed with metamorphr if installed via install.packages().
  # check if it installed first
  # also check, if pak is installed
  if (!is_installed_wrapper("pcaMethods")) {
    if (!is_installed_wrapper("pak")) {
      check_installed_wrapper("pak")
      check_installed_wrapper("pcaMethods")
    }
    check_installed_wrapper("pcaMethods")
  }
  n_pcs_int <- as.integer(n_pcs)
  if (n_pcs_int < 1 | is.na(n_pcs_int)) {
    rlang::abort(paste0("Argument n_pcs must be an integer (or a numeric) larger than 0, not `", as.character(n_pcs), "`."))
  }

  first_pc <- paste0("PC", as.character(pcs[[1]]))
  second_pc <- paste0("PC", as.character(pcs[[2]]))

  group_exists <- FALSE
  name_exists <- FALSE

  if (rlang::as_label(rlang::enquo(group_column)) != "NULL") {
    data_groups <- data %>%
      dplyr::select("Sample", {{ group_column }}) %>%
      dplyr::distinct()

    group_exists <- TRUE
  }

  if (rlang::as_label(rlang::enquo(name_column)) != "NULL") {
    data_names <- data %>%
      dplyr::select("UID", {{ name_column }}) %>%
      dplyr::distinct()

    name_exists <- TRUE
  }

  data <- data %>%
    dplyr::select("UID", "Sample", "Intensity") %>%
    tidyr::spread(key = "UID", value = "Intensity")

  data_samples <- data$Sample

  data <- data %>%
    dplyr::select(-"Sample") %>%
    as.matrix()

  rownames(data) <- data_samples

  pca_res <- pcaMethods::pca(data, method = method, nPcs = n_pcs_int, scale = "none", center = center, completeObs = TRUE)

  if (verbose == TRUE) {
    print(pca_res)
  }

  pca_scores <- pca_res@scores %>%
    tibble::as_tibble(rownames = "Sample")

  pca_loadings <- pca_res@loadings %>%
    tibble::as_tibble(rownames = "UID") %>%
    dplyr::mutate(UID = as.integer(.data$UID))

  if (what == "scores") {
    if (group_exists == TRUE) {
      pca_scores <- pca_scores %>%
        dplyr::left_join(data_groups, by = "Sample")
    }

    if (return_tbl == TRUE) {
      return(pca_scores)
    } else {
      if (group_exists == TRUE) {
        p <- ggplot2::ggplot(pca_scores, ggplot2::aes(.data[[first_pc]], .data[[second_pc]], color = {{ group_column }}))
      } else {
        p <- ggplot2::ggplot(pca_scores, ggplot2::aes(.data[[first_pc]], .data[[second_pc]]))
      }
      p <- p + ggplot2::xlab(first_pc) + ggplot2::ylab(second_pc) + ggplot2::labs(title = "Scores Plot")

    }
  } else if (what == "loadings") {
    if (name_exists == TRUE) {
      pca_loadings <- pca_loadings %>%
        dplyr::left_join(data_names, by = "UID")
    }

    if (return_tbl == TRUE) {
      return(pca_loadings)
    } else {
      p <- ggplot2::ggplot(pca_loadings, ggplot2::aes(.data[[first_pc]], .data[[second_pc]])) +
        ggplot2::xlab(first_pc) +
        ggplot2::ylab(second_pc) +
        ggplot2::labs(title = "Loadings Plot")

    }
  }

  p + ggplot2::geom_point()

}
