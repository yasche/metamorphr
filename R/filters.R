filter_global_mv <- function(data, max_na) {

  data %>%
    add_count(UID, wt = is.na(Intensity), name = "n_na") %>%
    group_by(UID) %>%
    mutate(perc_na = n_na / n()) %>%
    filter(perc_na <= max_na) %>%
    ungroup() %>%
    select(-n_na, -perc_na)

}

filter_grouped_mv <- function(data, grouping_column, max_na) {
  #using injection: https://rlang.r-lib.org/reference/topic-inject.html

  data %>%
    add_count(UID, {{ grouping_column }}, wt = is.na(Intensity), name = "n_na") %>%
    group_by(UID, {{ grouping_column }}) %>%
    mutate(perc_na = n_na / n()) %>%
    ungroup() %>%
    group_by(UID) %>%
    mutate(max_perc_na = max(perc_na)) %>%
    filter(max_perc_na <= max_na) %>%
    ungroup() %>%
    select(-n_na, - perc_na, -max_perc_na)

}

filter_cv <- function(data, reference_sample, max_cv = 0.2, na_as_zero = TRUE) {

  if (na_as_zero == TRUE) {
    data <- data %>%
      mutate(Intensity = case_when(is.na(Intensity) ~ 0,
                                   .default = Intensity))
  }

  data %>%
    mutate(Intensity_ref = case_when(Sample == reference_sample ~ Intensity,
                                     .default = NA)) %>%
    group_by(UID) %>%
    mutate(cv = sd(Intensity_ref, na.rm = TRUE) / mean(Intensity_ref, na.rm = TRUE)) %>%
    filter(cv <= max_cv) %>%
    ungroup() %>%
    mutate(Intensity = na_if(Intensity, 0)) %>%
    select(-Intensity_ref, -cv)

}

filter_blank <- function(data, blank_sample, min_frac = 3) {

  # substitute NA with 0 for better handling:
  # 0/0 = NaN
  # 1/0 = Inf
  # 0/1 = 0

  # primitive test:
  # tibble(frac_sb = c(0, 1, Inf, NaN, 10)) %>% filter(frac_sb >= 3 & !is.nan(frac_sb))

  data <- data %>%
    mutate(Intensity = case_when(is.na(Intensity) ~ 0,
                                 .default = Intensity))

  data %>%
    group_by(UID) %>%
    mutate(max_blank = case_when(Sample == blank_sample ~ Intensity,
                                 .default = NA),
           max_blank = max(max_blank, na.rm = TRUE),
           max_sample = case_when(Sample != blank_sample ~ Intensity,
                                  .default = NA),
           max_sample = max(max_sample, na.rm = TRUE),
           frac_sb = max_sample / max_blank) %>%
    filter(frac_sb >= min_frac & !is.nan(frac_sb)) %>%
    ungroup() %>%
    mutate(Intensity = na_if(Intensity, 0)) %>%
    select(-frac_sb, -max_blank, -max_sample)

}
