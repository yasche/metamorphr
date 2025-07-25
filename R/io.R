#' Read a feature table into a tidy tibble
#'
#' @description
#' Basically a wrapper around `readr::read_delim()` but performs some initial tidying operations such as `gather()` rearranging columns. The `label_col` will be renamed to _Feature_.
#'
#' @param file A path to a file but can also be a connection or literal data.
#' @param delim The field separator or delimiter. For example "," in csv files.
#' @param label_col The index or name of the column that will be used to label Features. For example an identifier (_e.g._, KEGG, CAS, HMDB) or a _m/z_-RT pair.
#' @param metadata_cols The index/indices or name(s) of column(s) that hold additional feature metadata (_e.g._, retention times, additional identifiers or _m/z_ values).
#' @param ... Additional arguments passed on to `readr::read_delim()`
#'
#' @return A tidy tibble.
#' @export
#'
#' @references \itemize{
#'  \item H. Wickham, \emph{J. Stat. Soft.} \strong{2014}, \emph{59}, DOI \href{https://doi.org/10.18637/jss.v059.i10}{10.18637/jss.v059.i10}.
#'  \item H. Wickham, M. Averick, J. Bryan, W. Chang, L. McGowan, R. François, G. Grolemund, A. Hayes, L. Henry, J. Hester, M. Kuhn, T. Pedersen, E. Miller, S. Bache, K. Müller, J. Ooms, D. Robinson, D. Seidel, V. Spinu, K. Takahashi, D. Vaughan, C. Wilke, K. Woo, H. Yutani, \emph{JOSS} \strong{2019}, \emph{4}, 1686, DOI \href{https://doi.org/10.21105/joss.01686}{10.21105/joss.01686}.
#'  \item “12 Tidy data | R for Data Science,” can be found under \url{https://r4ds.had.co.nz/tidy-data.html}, \strong{2023}.
#' }
#'
#' @examples
#' # Read a toy dataset in the format produced with Bruker MetaboScape (Version 2021).
#' featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
#'
#' # Example 1: Provide indices for metadata_cols
#' featuretable <- read_featuretable(featuretable_path, metadata_cols = 2:5)
#'
#' featuretable
#'
#' # Example 2: Provide a name for label_col and indices for metadata_cols
#' featuretable <- read_featuretable(
#'   featuretable_path,
#'   label_col = "m/z",
#'   metadata_cols = c(1, 2, 4, 5)
#' )
#'
#' featuretable
#'
#' # Example 3: Provide names for both, label_col and metadata_cols
#' featuretable <- read_featuretable(
#'   featuretable_path,
#'   label_col = "m/z",
#'   metadata_cols = c("Bucket label", "RT", "Name", "Formula")
#' )
#'
#' featuretable
#'
read_featuretable <- function(file, delim = ",", label_col = 1, metadata_cols = NULL, ...) {
  # perform some checks
  if (length(label_col) > 1) {
    stop("label_col must be of length 1.")
  }


  # if (!is.null(drop_cols)) {
  #  if (label_col %in% drop_cols) {
  #    stop("label_col can not be dropped.")
  #  }
  # }


  data <- readr::read_delim(file = file, delim = delim, show_col_types = FALSE, ...)
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
  data <- data %>%
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

  data
}


#' Create a blank metadata skeleton
#'
#' @description
#' Takes a tidy tibble created by `metamorphr::read_featuretable()` and returns an empty tibble for sample metadata. The tibble can either be populated directly in R or exported and edited by hand (_e.g._ in Excel). Metadata are necessary for several downstream functions. __More columns may be added if necessary__.
#'
#' @param data A tidy tibble created by `metamorphr::read_featuretable()`.
#'
#' @return An empty tibble structure with the necessary columns for metadata:
#' \describe{
#'   \item{Sample}{The sample name}
#'   \item{Group}{To which group does the samples belong? For example a treatment or a background. Note that additional columns with additional grouping information can be freely added if necessary.}
#'   \item{Replicate}{If multiple technical replicates exist in the data set,
#'   they must have the same value for Replicate and the same value for Group so that they can be collapsed.
#'   Examples for technical replicates are: the same sample was injected multiple times or workup was performed multiple times with the same starting material.
#'   If no technical replicates exist, set `Replicate = 1` for all samples.}
#'   \item{Batch}{The batch in which the samples were prepared or measured. If only one batch exists, set `Batch = 1` for all samples.}
#'   \item{Factor}{A sample-specific factor, for example dry weight or protein content.}
#'   ...
#' }
#' @export
#'
#' @examples featuretable_path <- system.file("extdata", "toy_metaboscape.csv", package = "metamorphr")
#' metadata <- read_featuretable(featuretable_path, metadata_cols = 2:5) %>%
#'   create_metadata_skeleton()
create_metadata_skeleton <- function(data) {
  sample_names <- data %>%
    dplyr::select("Sample") %>%
    dplyr::pull() %>%
    unique()

  metadata <- tibble::tibble(
    Sample = sample_names,
    Group = NA,
    Replicate = NA,
    Batch = NA,
    Factor = 1
  )

  metadata
}

#' Read a MGF file into a tidy tibble
#'
#' @description
#' \href{https://www.matrixscience.com/help/data_file_help.html}{MGF files} allow the storage of MS/MS spectra. With this
#' function they can be read into a tidy tibble. Each variable is stored in a column and each ion (observation) is stored in a separate row.
#' MS/MS spectra are stored in a list column named MSn. <br>
#' Please note that \href{https://fiehnlab.ucdavis.edu/projects/lipidblast/mgf-files}{MGF files are software-specific} so the variables
#' and their names may vary. This function was developed with the GNPS file format exported from \href{https://mzio.io/mzmine-news/}{mzmine} in mind.
#'
#'
#' @param file The path to the MGF file.
#' @param show_progress A `logical` indicating whether the progress of the import should be printed to the console. Only important for large MGF files.
#'
#' @return A tidy tibble holding MS/MS spectra.
#' @export
#'
#' @examples
#' mgf_path <- system.file("extdata", "toy_mgf.mgf", package = "metamorphr")
#' read_mgf(mgf_path)
read_mgf <- function(file, show_progress = TRUE) {
  mgf_string <- readr::read_lines(file)

  begin_ions <- grep("^BEGIN IONS$", mgf_string)
  end_ions <- grep("^END IONS$", mgf_string)

  result_list <- purrr::map2(begin_ions, end_ions, function(start, end, mgf_string) {mgf_string[start:end]}, mgf_string) %>%
    purrr::map(internal_mgf_to_data_metadata, .progress = show_progress) %>%
    dplyr::bind_rows() %>%
    dplyr::relocate("MSn", .after = -1) %>%
    readr_type_convert_quiet()

  result_list$result
}
