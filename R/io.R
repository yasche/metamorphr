read_featuretable <- function(file, delim = ",", label_col = 1, drop_cols = NULL, ...) {

  # perform some checks
  if (length(label_col) > 1) {
    stop("label_col must be of length 1.")
  }

  if (!is.null(drop_cols)) {
    if (label_col %in% drop_cols) {
      stop("label_col can not be dropped.")
    }
  }


  #renamed Measurement -> Sample; label -> Metabolite
  data <- read_delim(file = file, delim = delim, ...) %>%
    rename("Metabolite" = label_col) %>%
    select(- {{ drop_cols }}) %>%
    mutate(Metabolite = as.character(Metabolite)) %>%
    mutate(UID = seq(1:nrow(.))) %>%
    relocate(UID, .before = Metabolite) %>%
    gather(c(-1, -2), key = "Sample", value = "Intensity") %>%
    mutate(Intensity = as.numeric(Intensity)) %>%
    #replace 0 with NA
    mutate(Intensity = na_if(Intensity, 0))

  data
}


create_blank_metadata <- function(data) {

  # creates blank tibble for metadata with required columns
  # additional columns may be added

  sample_names <- data %>%
    select(Sample) %>%
    pull() %>%
    unique()

  metadata <- tibble(
    Sample = sample_names,
    Group = NA,
    Replicate = NA,
    Batch = NA,
    Factor = 1
  )

  metadata
}
