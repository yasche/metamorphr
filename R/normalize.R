#' Normalize intensities across samples by dividing by the sample median
#'
#' @description
#' Normalize across samples by dividing feature intensities by the sample median, making the median 1 in all samples.
#' See References for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references T. Ramirez, A. Strigun, A. Verlohner, H.-A. Huener, E. Peter, M. Herold, N. Bordag, W. Mellert, T. Walk, M. Spitzer, X. Jiang, S. Sperber, T. Hofmann, T. Hartung, H. Kamp, B. Van Ravenzwaay, \emph{Arch Toxicol} \strong{2018}, \emph{92}, 893–906, DOI 10.1007/s00204-017-2079-6.
#'
#' @examples
#' toy_metaboscape %>%
#'   normalize_median()
normalize_median <- function(data) {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Intensity = .data$Intensity / stats::median(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup()
}

#' Normalize intensities across samples by dividing by the sample sum
#'
#' @description
#' Normalize across samples by dividing feature intensities by the sum of all intensities in a sample, making the sum 1 in all samples.
#'
#' \strong{Important Note}
#'
#' Intensities of individual features will be very small after this normalization approach. It is therefore advised to multiply all intensities with a fixed number (e.g., 1000) after normalization.
#' See \href{https://omicsforum.ca/t/sum-normalization-needs-clarification-or-potentially-has-an-issue/3244}{this discussion on OMICSForum.ca} and the examples below
#' for further information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @examples
#' # Example 1: Normalization only
#' toy_metaboscape %>%
#'   normalize_sum()
#'
#' # Example 2: Multiply with 1000 after normalization
#' toy_metaboscape %>%
#'   normalize_sum() %>%
#'   dplyr::mutate(Intensity = .data$Intensity * 1000)
normalize_sum <- function(data) {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Intensity = .data$Intensity / sum(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup()
}

#' Normalize intensities across samples using standard Quantile Normalization
#'
#' @description
#' This is the standard approach for Quantile Normalization. Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_group}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, \emph{Sci Rep} \strong{2020}, \emph{10}, 15534, DOI 10.1038/s41598-020-72664-6.
#'
#' @examples
#' toy_metaboscape %>%
#'   normalize_quantile_all()
normalize_quantile_all <- function(data) {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # calculate mean of ties
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie")
}

#' Normalize intensities across samples using grouped Quantile Normalization
#'
#' @description
#' This function performs a Quantile Normalization on each sub-group in the data set. \strong{It therefore requires grouping information}. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue).
#'
#' Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_all}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#' Note that it is equivalent to the 'Class-specific' normalization in Zhao \emph{et al.} but has been renamed for internal consistency.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, \emph{Sci Rep} \strong{2020}, \emph{10}, 15534, DOI 10.1038/s41598-020-72664-6.
#'
#' @examples
#' toy_metaboscape %>%
#'   # Metadata, including grouping information, must be added before using normalize_quantile_group()
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   normalize_quantile_group(group_column = Group)
normalize_quantile_group <- function(data, group_column = .data$Group) {
  # also called class-specific; named group to make it consistent
  data %>%
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # calculate mean of ties
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie")
}

#' Normalize intensities across samples using grouped Quantile Normalization with multiple batches
#'
#' @description
#' This function performs a Quantile Normalization on each sub-group and batch in the data set. \strong{It therefore requires grouping information}. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue).
#'
#' Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_all}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#' Note that it is equivalent to the 'Discrete' normalization in Zhao \emph{et al.} but has been renamed for internal consistency.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch_column Which column contains the batch information? Usually `grouping_column = Batch`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, \emph{Sci Rep} \strong{2020}, \emph{10}, 15534, DOI 10.1038/s41598-020-72664-6.
#'
#' @examples
#' toy_metaboscape %>%
#'   # Metadata, including grouping and batch information,
#'   # must be added before using normalize_quantile_batch()
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   normalize_quantile_batch(group_column = Group, batch_column = Batch)
normalize_quantile_batch <- function(data, group_column = .data$Group, batch_column = .data$Batch) {
  data %>%
    dplyr::group_by({{ group_column }}, {{ batch_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, {{ batch_column }}, .data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # calculate mean of ties
    dplyr::group_by({{ group_column }}, {{ batch_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, {{ batch_column }}, .data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie")
}

#' Normalize intensities across samples using smooth Quantile Normalization (qsmooth)
#'
#' @description
#' This function performs a smooth Quantile Normalization on each sub-group in the data set (qsmooth). \strong{It therefore requires grouping information}. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue). The result lies somewhere between \code{\link[metamorphr]{normalize_quantile_group}}
#' and \code{\link[metamorphr]{normalize_quantile_all}}. Basically a re-implementation of Hicks \emph{et al.} (2018).
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param rolling_window `normalize_quantile_smooth` uses a rolling window median to eliminate isolated outliers. This argument specifies the size of the rolling window as a fraction of the number of unique features in `data`. For example, if there are 100 features in `data` and `rolling_window = 0.05`, the rolling median will be calculated from 5 features. Set `rolling_window = 0` to disable.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references \itemize{
#'  \item S. C. Hicks, K. Okrah, J. N. Paulson, J. Quackenbush, R. A. Irizarry, H. C. Bravo, \emph{Biostatistics} \strong{2018}, \emph{19}, 185–198, DOI 10.1093/biostatistics/kxx028.
#'  \item Y. Zhao, L. Wong, W. W. B. Goh, \emph{Sci Rep} \strong{2020}, \emph{10}, 15534, DOI 10.1038/s41598-020-72664-6.
#' }
#'
#' @examples
#' toy_metaboscape %>%
#'   # Metadata, including grouping information, must be added before using normalize_quantile_group()
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   normalize_quantile_smooth(group_column = Group)
normalize_quantile_smooth <- function(data, group_column = .data$Group, rolling_window = 0.05) {
  if (rolling_window > 1 | rolling_window < 0) {
    rlang::abort(paste0("rolling_window must be between 0 and 1, not ", as.character(rolling_window), "."))
  }

  k <- floor(length(unique(data$UID)) * rolling_window)

  if (k %% 2 == 0) {
    k <- k + 1
  }

  n_rows <- nrow(data)

  data %>%
    #add row id to preserve row order
    dplyr::mutate(orig_row_id = 1:.env$n_rows) %>%
    dplyr::mutate(orig_Intensity = .data$Intensity) %>%
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(Froof_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Rank) %>%
    dplyr::mutate(Fline_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::mutate(Fi_Intensity = .data$Intensity) %>%
    # dplyr::select(-"Intensity") %>%
    dplyr::group_by(.data$Rank) %>%
    dplyr::mutate(
      SST = sum((.data$Fi_Intensity - .data$Fline_Intensity)^2),
      SSB = sum((.data$Froof_Intensity - .data$Fline_Intensity)^2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$Sample, .data$Rank) %>%
    dplyr::mutate(w1 = 1 - .data$SSB / .data$SST) %>%
    # this was added as it is also present in the original qstats() source code; prevents division by 0
    dplyr::mutate(w1 = dplyr::case_when(.data$SST < 1e-06 ~ 1,
      .default = .data$w1
    )) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(w2 = stats::runmed(.data$w1, k = .env$k, endrule = "constant")) %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(Intensity = .data$w2 * .data$Fline_Intensity + (1 - .data$w2) * .data$Froof_Intensity) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"Froof_Intensity", -"Fline_Intensity", -"Fi_Intensity", -"SST", -"SSB", -"w1", -"w2") %>%
    # perform QN on smoothed data
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # calculate mean of ties
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    # min -> first
    dplyr::mutate(Rank = rank(.data$orig_Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #dplyr::arrange(.data$UID) %>%
    dplyr::arrange(.data$orig_row_id) %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie", -"orig_Intensity", -"orig_row_id")
}

#' Normalize intensities across samples using a reference feature
#'
#' @description
#' Performs a normalization based on a reference feature, for example an internal standard.
#' Divides the Intensities of all features by the Intensity of the reference feature in that sample and multiplies them with a constant value, making the Intensity
#' of the reference feature the same in each sample.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param reference_feature An identifier for the reference feature. Must be unique. It is recommended to use the UID.
#' @param identifier_column The column in which to look for the reference feature. It is recommended to use `identifier_column = UID`
#' @param reference_feature_intensity Either a constant value with which the intensity of each feature is multiplied or a function (e.g., mean, median, min, max).
#' If a function is provided, it will use that function on the Intensities of the reference feature in all samples before normalization and multiply the intensity of each feature with that value after dividing by the Intensity of the reference feature.
#' For example, if `reference_feature_intensity = mean`, it calculates the mean of the Intensities of the reference features across samples before normalization. It then divides the Intensity of each feature by the Intensity of the reference feature in that sample.
#' Finally, it multiplies each Intensity with the mean of the Intensities of the reference features prior to normalization.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @examples
#' # Divide by the reference feature and make its Intensity 1000 in each sample
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = 1000)
#'
#' # Divide by the reference feature and make its Intensity the mean of intensities
#' # of the reference features before normalization
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = mean)
normalize_ref <- function(data, reference_feature, identifier_column, reference_feature_intensity = 1) {
  # check if reference_feature is unique
  # it has to be unique because .data$Intensity will be divided by the reference feature.
  multiple_ids <- data %>%
    dplyr::select("Sample", {{ identifier_column }}) %>%
    dplyr::filter({{ identifier_column }} == reference_feature) %>%
    dplyr::select("Sample") %>%
    dplyr::pull() %>%
    table()

  ref_ints <- data %>%
    dplyr::select("Sample", "Intensity", {{ identifier_column }}) %>%
    dplyr::filter({{ identifier_column }} == reference_feature)

  if (length(multiple_ids) == 0) {
    rlang::abort(paste0("\n\nreference_feature must occur exactly once in each sample.\nThere is no feature that matches '", reference_feature, "' in column ", rlang::expr_label(substitute(identifier_column)), ".\nDid you make a typo?"))
  }

  if (max(multiple_ids) > 1) {
    which_uids <- data %>%
      dplyr::filter({{ identifier_column }} == reference_feature) %>%
      dplyr::pull("UID") %>%
      unique()

    rlang::abort(paste0("\n\nreference_feature must occur exactly once in each sample.\nThere are ", as.character(max(multiple_ids)), " features that match '", reference_feature, "' in column ", rlang::expr_label(substitute(identifier_column)), ".\nIt is recommended to use the UID column to refer to specific features:\nYou may use `identifier_column = UID` and set the `reference_feature` argument to the correct UID of the following: ", paste(as.character(which_uids), collapse = " "), "."))
  }

  if (any(is.na(ref_ints$Intensity))) {
    ref_ints_na <- ref_ints %>%
      dplyr::filter(is.na(.data$Intensity)) %>%
      dplyr::pull(.data$Sample)
    rlang::abort(paste0("\n\nThe intensity of ", reference_feature, " in Sample(s) ", paste(unique(ref_ints_na), collapse = ","), " is NA!\nPlease use any of the available 'impute_' functions first.\nStart typing 'metamorphr::impute_' in the console to see the available options."))
  }

  if (typeof(reference_feature_intensity) == "closure" | typeof(reference_feature_intensity) == "builtin") {
    reference_feature_intensity <- ref_ints %>%
      dplyr::select("Intensity") %>%
      dplyr::pull() %>%
      reference_feature_intensity()
  }

  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(ref_int = dplyr::case_when({{ identifier_column }} == reference_feature ~ .data$Intensity,
      .default = NA
    )) %>%
    dplyr::mutate(ref_int = mean(.data$ref_int, na.rm = T)) %>%
    dplyr::mutate(Intensity = .data$Intensity / .data$ref_int * .env$reference_feature_intensity) %>%
    dplyr::select(-"ref_int") %>%
    dplyr::ungroup()
}

#' Normalize intensities across samples using a normalization factor
#'
#' @description
#' Normalization is done by dividing the intensity by a sample-specific factor (e.g., weight, protein or DNA content).
#' This function requires a sample-specific factor, usually supplied via the `Factor` column from the sample metadata.
#' See the Examples section for details.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param factor_column Which column contains the sample-specific factor? Usually `factor_column = Factor`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @returns A tibble with intensities normalized across samples.
#' @export
#'
#' @examples
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   normalize_factor()
normalize_factor <- function(data, factor_column = .data$Factor) {
  data %>%
    dplyr::mutate(Intensity = .data$Intensity / {{ factor_column }})
}


#' Normalize intensities across samples using cyclic LOESS normalization
#'
#' @description
#' The steps the algorithm takes are the following:
#'
#' \enumerate{
#'  \item log2 transform the intensities
#'  \item Choose 2 samples to generate an \href{https://en.wikipedia.org/wiki/MA_plot}{MA-plot} from
#'  \item Fit a LOESS curve
#'  \item Subtract half of the difference between the predicted value and the true value from the intensity of sample 1 and add the same amount to the intensity of Sample 2
#'  \item Repeat for all unique combinations of samples
#'  \item Repeat all steps until the model converges or \code{n_iter} is reached.
#' }
#'
#' Convergence is assumed if the confidence intervals of all LOESS smooths include the 0 line. If `fixed_iter = TRUE`, the algorithm will perform exactly `n_iter` iterations.
#' If `fixed_iter = FALSE`, the algorithm will perform a maximum of `n_iter` iterations.
#'
#' See the reference section for details.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param n_iter The number of iterations to perform. If `fixed_iter = TRUE` exactly `n_iter` will be performed. If `fixed_iter = FALSE` a maximum of `n_iter` will be performed and the algorithm will stop whether convergence is reached or not.
#' @param fixed_iter Should a fixed number of iterations be performed?
#' @param loess_span The span of the LOESS fit. A larger span produces a smoother line.
#' @param level The confidence level for the convergence criterion. Note that a a larger confidence level produces larger confidence intervals and therefore the algorithm stops earlier.
#' @param verbose `TRUE` or `FALSE`. Should messages be printed to the console?
#' @param ... Arguments passed onto \code{\link[stats]{loess}}. For example, `degree = 1, family = "symmetric", iterations = 4, surface = "direct"` produces a LOWESS fit.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references \itemize{
#'  \item B. M. Bolstad, R. A. Irizarry, M. Åstrand, T. P. Speed, \emph{Bioinformatics} \strong{2003}, \emph{19}, 185–193, DOI 10.1093/bioinformatics/19.2.185.
#'  \item Karla Ballman, Diane Grill, Ann Oberg, Terry Therneau, “Faster cyclic loess: normalizing DNA arrays via linear models” can be found under https://www.mayo.edu/research/documents/biostat-68pdf/doc-10027897, 2004.
#'  \item K. V. Ballman, D. E. Grill, A. L. Oberg, T. M. Therneau, \emph{Bioinformatics} \strong{2004}, \emph{20}, 2778–2786, DOI 10.1093/bioinformatics/bth327.
#' }
#'
#' @examples
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   normalize_cyclic_loess()
normalize_cyclic_loess <- function(data, n_iter = 3, fixed_iter = TRUE, loess_span = 0.7, level = 0.95, verbose = FALSE, ...) {
  model_conv = FALSE
  combinations <- data$Sample %>%
    unique() %>%
    length() %>%
    utils::combn(2, simplify = F)
  data_cyclo <- dplyr::mutate(data, Intensity = log2(.data$Intensity))
  data_list <- internal_prep_pca_imputes(data_cyclo, direction = 1)
  data_cyclo <- data_list$data
  #data_cyclo_last_iter <- data_cyclo
  for(h in 1:n_iter) {
    curr_conv <- logical()
    for(i in 1:length(combinations)) {
      if (fixed_iter == TRUE) {
        break
      }
      data_cyclo_curr <- tibble::tibble(
        M = unname(data_cyclo[, combinations[[i]][1]] - data_cyclo[, combinations[[i]][2]]),
        A = 0.5 * unname(data_cyclo[, combinations[[i]][1]] + data_cyclo[, combinations[[i]][2]])
      )
      curr_loess <- stats::loess(M ~ A, data = data_cyclo_curr, span = loess_span, ...)
      data_cyclo_curr2 <- data.frame(A = data_cyclo_curr$A)
      curr_pred <- stats::predict(curr_loess, newdata = data_cyclo_curr2, se = TRUE)
      t_val <- stats::qt(1 - ((1 - level) / 2), curr_pred$df)
      curr_pred <- as.data.frame(curr_pred)
      curr_pred <- dplyr::mutate(curr_pred,
                                 lower_conv = .data$fit - .env$t_val * .data$se.fit,
                                 upper_conv = .data$fit + .env$t_val * .data$se.fit)
      curr_conv <- append((all(curr_pred$lower_conv < 0) & all(curr_pred$upper_conv > 0)), curr_conv)
    }
    model_conv <- all(curr_conv)
    if(model_conv == TRUE & fixed_iter == FALSE) {
      break
    }
    for(i in 1:length(combinations)) {
      data_cyclo_curr <- tibble::tibble(
        M = unname(data_cyclo[, combinations[[i]][1]] - data_cyclo[, combinations[[i]][2]]),
        A = 0.5 * unname(data_cyclo[, combinations[[i]][1]] + data_cyclo[, combinations[[i]][2]])
      )
      curr_loess <- stats::loess(M ~ A, data = data_cyclo_curr, span = loess_span, ...)
      data_cyclo_curr2 <- data.frame(A = data_cyclo_curr$A)
      curr_pred <- stats::predict(curr_loess, newdata = data_cyclo_curr2)
      data_cyclo[, combinations[[i]][1]] <- data_cyclo[, combinations[[i]][1]] - unname(0.5 * curr_pred)
      data_cyclo[, combinations[[i]][2]] <- data_cyclo[, combinations[[i]][2]] + unname(0.5 * curr_pred)
    }
    #rmsd <- sqrt(1/length(data_cyclo) * sum((data_cyclo - data_cyclo_last_iter)^2))
    #print(rmsd)
    #data_cyclo_last_iter <- data_cyclo
    if (verbose == TRUE) {
      rlang::inform(paste0("Finished iteration ", h, "."))
    }
  }
  data_cyclo <- 2^data_cyclo
  data_list$data <- data_cyclo
  if (verbose == TRUE) {
    if (model_conv == FALSE) {
      rlang::inform(paste0("No convergence was reached after ", n_iter, " iterations."))
    } else {
      if (fixed_iter == TRUE) {
        rlang::inform(paste0("Successfully ran ", n_iter, " iterations."))
      } else {
        rlang::inform(paste0("Reached convergence after ", h - 1, " iterations."))
      }
    }
  }
  internal_clean_pca_results(data_list = data_list, direction = 1)
}

#' Normalize intensities across samples using a Probabilistic Quotient Normalization (PQN)
#'
#' This method was originally developed for H-NMR spectra of complex biofluids but has been adapted for other 'omics data. It aims to eliminate
#' dilution effects by calculating the most probable dilution factor for each sample, relative to one or more reference samples. See references for more details.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param fn Which function should be used to calculate the reference spectrum from the reference samples? Can be either "mean" or "median".
#' @param normalize_sum A logical indicating whether a sum normalization (aka total area normalization) should be performed prior to PQN.
#' It is \href{https://rdrr.io/github/ricoderks/Rcpm/man/pqn.html}{recommended} to do so and other packages (e.g., \CRANpkg{KODAMA}) also perform a sum normalization prior to PQN.
#' @param reference_samples Either `NULL` or a character or character vector containing the sample(s)
#' to calculate the reference spectrum from. In the original publication, it is advised to calculate the median of control samples.
#' If `NULL`, all samples will be used to calculate the reference spectrum.
#' @param ref_as_group A logical indicating if `reference_samples` are the names of samples or group(s).
#' @param group_column Only relevant if `ref_as_group = TRUE`. Which column should be used for grouping reference and non-reference samples? Usually `group_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references \itemize{
#' \item F. Dieterle, A. Ross, G. Schlotterbeck, H. Senn, \emph{Anal. Chem.} \strong{2006}, \emph{78}, 4281–4290, DOI 10.1021/ac051632c.
#' }
#'
#' @examples
#' # specify the reference samples with their sample names
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   normalize_pqn(reference_samples = c("QC1", "QC2", "QC3"))
#'
#' # specify the reference samples with their group names
#' toy_metaboscape %>%
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   impute_lod() %>%
#'   normalize_pqn(reference_samples = c("QC"), ref_as_group = TRUE, group_column = Group)
normalize_pqn <- function(data, fn = "median", normalize_sum = TRUE, reference_samples = NULL, ref_as_group = FALSE, group_column = NULL) {
  if (normalize_sum == TRUE) {
    data <- normalize_sum(data)
  }

  if (ref_as_group == TRUE) {
    if (is.null(reference_samples)) {
      reference_samples <- data %>%
        dplyr::pull({{ group_column }}) %>%
        unique()
    }
    data <- data %>%
      dplyr::mutate(Ref_Int = dplyr::case_when({{ group_column }} %in% .env$reference_samples ~ .data$Intensity,
        .default = NA
      )) %>%
      dplyr::group_by(.data$UID)
  } else {
    if (is.null(reference_samples)) {
      reference_samples <- data %>%
        dplyr::pull("Sample") %>%
        unique()
    }
    data <- data %>%
      dplyr::mutate(Ref_Int = dplyr::case_when(.data$Sample %in% .env$reference_samples ~ .data$Intensity,
        .default = NA
      )) %>%
      dplyr::group_by(.data$UID)
  }
  if (fn == "median") {
    data <- data %>%
      dplyr::mutate(Ref_Int = stats::median(.data$Ref_Int, na.rm = TRUE))
  } else if (fn == "mean") {
    data <- data %>%
      dplyr::mutate(Ref_Int = mean(.data$Ref_Int, na.rm = TRUE))
  } else {
    rlang::abort('Argument fn must be "median" or "mean"')
  }

  data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Intensity = .data$Intensity / stats::median(.data$Intensity / .data$Ref_Int, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Ref_Int")
}
