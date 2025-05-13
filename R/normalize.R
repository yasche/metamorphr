#' Normalize intensities across samples by dividing by the sample median
#'
#' @description
#' Normalize across samples by dividing feature intensities by the sample median, making the median 1 in all samples.
#' See References for more information.
#'
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references T. Ramirez, A. Strigun, A. Verlohner, H.-A. Huener, E. Peter, M. Herold, N. Bordag, W. Mellert, T. Walk, M. Spitzer, X. Jiang, S. Sperber, T. Hofmann, T. Hartung, H. Kamp, B. Van Ravenzwaay, <i>Arch Toxicol</i> <b>2018</b>, <i>92</i>, 893–906, DOI <a href = "https://doi.org/10.1007/s00204-017-2079-6">10.1007/s00204-017-2079-6</a>.
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
#' <b>Important Note</b><br>
#' Intensities of individual features will be very small after this normalization approach. It is therefore advised to multiply all intensities with a fixed number (e.g., 1000) after normalization.
#' See <a href = "https://omicsforum.ca/t/sum-normalization-needs-clarification-or-potentially-has-an-issue/3244">this discussion on OMICSForum.ca</a> and the examples below
#' for further information.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @examples
#' #Example 1: Normalization only
#' toy_metaboscape %>%
#'   normalize_sum()
#'
#' #Example 2: Multiply with 1000 after normalization
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
#' @references Y. Zhao, L. Wong, W. W. B. Goh, <i>Sci Rep</i> <b>2020</b>, <i>10</i>, 15534, DOI <a href = "https://doi.org/10.1038/s41598-020-72664-6">10.1038/s41598-020-72664-6</a>.
#'
#' @examples
#' toy_metaboscape %>%
#'   normalize_quantile_all()
normalize_quantile_all <- function(data)  {
  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #calculate mean of ties
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
#' This function performs a Quantile Normalization on each sub-group in the data set. <b>It therefore requires grouping information</b>. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue).
#'
#' Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_all}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#' Note that it is equivalent to the 'Class-specific' normalization in Zhao <i>et al.</i> but has been renamed for internal consistency.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, <i>Sci Rep</i> <b>2020</b>, <i>10</i>, 15534, DOI <a href = "https://doi.org/10.1038/s41598-020-72664-6">10.1038/s41598-020-72664-6</a>.
#'
#' @examples
#' toy_metaboscape %>%
#' #Metadata, including grouping information, must be added before using normalize_quantile_group()
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   normalize_quantile_group(group_column = Group)
normalize_quantile_group <- function(data, group_column = .data$Group) {
  #also called class-specific; named group to make it consistent
  data %>%
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #calculate mean of ties
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
#' This function performs a Quantile Normalization on each sub-group and batch in the data set. <b>It therefore requires grouping and batch information</b>. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue).
#'
#' Other sub-flavors are also available:
#' - \code{\link[metamorphr]{normalize_quantile_all}}
#' - \code{\link[metamorphr]{normalize_quantile_batch}}
#' - \code{\link[metamorphr]{normalize_quantile_smooth}}
#'
#' See References for more information.
#' Note that it is equivalent to the 'Discrete' normalization in Zhao <i>et al.</i> but has been renamed for internal consistency.
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param batch_column Which column contains the batch information? Usually `grouping_column = Batch`. Uses \code{\link[rlang]{args_data_masking}}.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references Y. Zhao, L. Wong, W. W. B. Goh, <i>Sci Rep</i> <b>2020</b>, <i>10</i>, 15534, DOI <a href = "https://doi.org/10.1038/s41598-020-72664-6">10.1038/s41598-020-72664-6</a>.
#'
#' @examples
#' toy_metaboscape %>%
#' #Metadata, including grouping and batch information,
#' #must be added before using normalize_quantile_batch()
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
    #calculate mean of ties
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
#' This function performs a smooth Quantile Normalization on each sub-group in the data set (qsmooth). <b>It therefore requires grouping information</b>. See
#' Examples for more information. This approach might perform better than the standard approach, \code{\link[metamorphr]{normalize_quantile_all}},
#' if sub-groups are very different (e.g., when comparing cancer vs. normal tissue). The result lies somewhere between \code{\link[metamorphr]{normalize_quantile_group}}
#' and \code{\link[metamorphr]{normalize_quantile_all}}. Basically a re-implementation of Hicks <i>et al.</i> (2018).
#'
#' @param data A tidy tibble created by \code{\link[metamorphr]{read_featuretable}}.
#' @param group_column Which column should be used for grouping? Usually `grouping_column = Group`. Uses \code{\link[rlang]{args_data_masking}}.
#' @param rolling_window `normalize_quantile_smooth` uses a rolling window median to eliminate isolated outliers. This argument specifies the size of the rolling window as a fraction of the number of unique features in `data`. For example, if there are 100 features in `data` and `rolling_window = 0.05`, the rolling median will be calculated from 5 features. Set `rolling_window = 0` to disable.
#'
#' @return A tibble with intensities normalized across samples.
#' @export
#'
#' @references For further information, see
#' <ul>
#'  <li>S. C. Hicks, K. Okrah, J. N. Paulson, J. Quackenbush, R. A. Irizarry, H. C. Bravo, <i>Biostatistics</i> <b>2018</b>, <i>19</i>, 185–198, DOI <a href = "https://doi.org/10.1093/biostatistics/kxx028">10.1093/biostatistics/kxx028</a></li>
#'  <li>Y. Zhao, L. Wong, W. W. B. Goh, <i>Sci Rep</i> <b>2020</b>, <i>10</i>, 15534, DOI <a href = "https://doi.org/10.1038/s41598-020-72664-6">10.1038/s41598-020-72664-6</a>.
#' </ul>
#'
#' @examples
#' toy_metaboscape %>%
#' #Metadata, including grouping information, must be added before using normalize_quantile_group()
#'   join_metadata(toy_metaboscape_metadata) %>%
#'   normalize_quantile_smooth(group_column = Group)
normalize_quantile_smooth <- function(data, group_column = .data$Group, rolling_window = 0.05) {

  if(rolling_window > 1 | rolling_window < 0) {
    stop(paste0("rolling_window must be between 0 and 1, not ", as.character(rolling_window), "."))
  }

  k = floor(length(unique(data$UID)) * rolling_window)

  if(k %% 2 == 0) {
    k <- k + 1
  }

  data %>%
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
    #dplyr::select(-"Intensity") %>%
    dplyr::group_by(.data$Rank) %>%
    dplyr::mutate(SST = sum((.data$Fi_Intensity - .data$Fline_Intensity)^2),
                  SSB = sum((.data$Froof_Intensity - .data$Fline_Intensity)^2)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$Sample, .data$Rank) %>%
    dplyr::mutate(w1 = 1 - .data$SSB / .data$SST) %>%
    #this was added as it is also present in the original qstats() source code; prevents division by 0
    dplyr::mutate(w1 = dplyr::case_when(.data$SST < 1e-06 ~ 1,
                                        .default = .data$w1)) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(w2 = stats::runmed(.data$w1, k = .env$k, endrule = "constant")) %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(Intensity = .data$w2 * .data$Fline_Intensity + (1 - .data$w2) * .data$Froof_Intensity) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"Froof_Intensity", -"Fline_Intensity", -"Fi_Intensity", -"SST", -"SSB", -"w1", -"w2") %>%
    #perform QN on smoothed data
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    dplyr::mutate(Rank = rank(.data$Intensity, ties.method = "first")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Rank) %>%
    dplyr::mutate(tmp_Intensity = mean(.data$Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #calculate mean of ties
    dplyr::group_by({{ group_column }}, .data$Sample) %>%
    #min -> first
    dplyr::mutate(Rank = rank(.data$orig_Intensity, ties.method = "min")) %>%
    dplyr::mutate(tie = vctrs::vec_duplicate_detect(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group_column }}, .data$Sample, .data$Rank) %>%
    dplyr::mutate(Intensity = mean(.data$tmp_Intensity, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Rank", -"tmp_Intensity", -"tie", -"orig_Intensity") %>%
    dplyr::arrange(.data$UID)
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
#' #Divide by the reference feature and make its Intensity 1000 in each sample
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = 1000)
#'
#' #Divide by the reference feature and make its Intensity the mean of intensities
#' #of the reference features before normalization
#' toy_metaboscape %>%
#'   impute_lod() %>%
#'   normalize_ref(reference_feature = 2, identifier_column = UID, reference_feature_intensity = mean)
normalize_ref <- function(data, reference_feature, identifier_column, reference_feature_intensity = 1) {

  #check if reference_feature is unique
  #it has to be unique because .data$Intensity will be divided by the reference feature.
  multiple_ids <- data %>%
    dplyr::select("Sample", {{ identifier_column }}) %>%
    dplyr::filter({{ identifier_column }} == reference_feature) %>%
    dplyr::select("Sample") %>%
    dplyr::pull() %>%
    table()

  ref_ints <- data %>%
    dplyr::select("Sample", "Intensity", {{ identifier_column }}) %>%
    dplyr::filter({{ identifier_column }} == reference_feature)

  if(length(multiple_ids) == 0) {
    stop(paste0("\n\nreference_feature must occur exactly once in each sample.\nThere is no feature that matches '", reference_feature, "' in column ", rlang::expr_label(substitute(identifier_column)), ".\nDid you make a typo?"))
  }

  if(max(multiple_ids) > 1) {
    which_uids <- data %>%
      dplyr::filter({{ identifier_column }} == reference_feature) %>%
      dplyr::pull("UID") %>%
      unique()

    stop(paste0("\n\nreference_feature must occur exactly once in each sample.\nThere are ", as.character(max(multiple_ids)), " features that match '", reference_feature, "' in column ", rlang::expr_label(substitute(identifier_column)), ".\nIt is recommended to use the UID column to refer to specific features:\nYou may use `identifier_column = UID` and set the `reference_feature` argument to the correct UID of the following: ", paste(as.character(which_uids), collapse = " "), "."))
  }

  if(any(is.na(ref_ints$Intensity))) {
    ref_ints_na <- ref_ints %>%
      dplyr::filter(is.na(.data$Intensity)) %>%
      dplyr::pull(.data$Sample)
    stop(paste0("\n\nThe intensity of ",  reference_feature, " in Sample(s) ", paste(unique(ref_ints_na), collapse = ","), " is NA!\nPlease use any of the available 'impute_' functions first.\nStart typing 'metamorphr::impute_' in the console to see the available options."))
  }

  if(typeof(reference_feature_intensity) == "closure" | typeof(reference_feature_intensity) == "builtin") {
    reference_feature_intensity <- ref_ints %>%
      dplyr::select("Intensity") %>%
      dplyr::pull() %>%
      reference_feature_intensity()
  }

  data %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(ref_int = dplyr::case_when({{ identifier_column }} == reference_feature ~ .data$Intensity,
                                             .default = NA)) %>%
    dplyr::mutate(ref_int = mean(.data$ref_int, na.rm = T)) %>%
    dplyr::mutate(Intensity = .data$Intensity / .data$ref_int * .env$reference_feature_intensity) %>%
    dplyr::select(-"ref_int") %>%
    dplyr::ungroup()

}

normalize_factor <- function() {

}

normalize_cyclic_loess <- function() {
  #also fast_loess?
}

#' Normalize intensities across samples using a Probabilistic Quantile Normalization
#'
#' @param data
#' @param ref_fn
#' @param reference_samples
#' @param ref_as_group
#' @param group_column
#'
#' @return
#' @export
#'
#' @examples
normalize_pqn <- function(data, ref_fn = "mean", reference_samples = NULL, ref_as_group = FALSE, group_column = NULL) {

}

#potential other:
#contrast, cubic_splines, lbs (linear baseline scaling), mstus, non-linear baseline normalization
