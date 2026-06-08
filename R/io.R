#' Read a feature table into a tidy tibble
#'
#' @description
#' Basically a wrapper around `readr::read_delim()` but performs some initial tidying operations such as `gather()` rearranging columns. The `label_col` will be renamed to _Feature_.
#'
#' @param file A path to a file but can also be a connection or literal data.
#' @param delim The field separator or delimiter. For example "," in csv files.
#' @param label_col The index or name of the column that will be used to label Features. For example an identifier (_e.g._, KEGG, CAS, HMDB) or a _m/z_-RT pair.
#' @param metadata_cols The index/indices or name(s) of column(s) that hold additional feature metadata (_e.g._, retention times, additional identifiers or _m/z_ values).
#' @param remove_empty_cols Either `TRUE` or `FALSE`. Should empty columns be removed after reading the feature table? For a more fine-grained control, you can use a combination of \code{\link[readr]{read_delim}}, \code{\link[metamorphr]{remove_empty_cols}} and \code{\link[metamorphr]{convert_from_wide}}.See the respective function documentation for more details.
#' @param show_removed_cols Only relevant if `remove_empty_cols = TRUE`. If `TRUE` prints a message that shows which columns were removed.
#' @param ... Additional arguments passed on to `readr::read_delim()`
#'
#' @return A tidy tibble.
#' @export
#'
#' @references \itemize{
#'  \item H. Wickham, \emph{J. Stat. Soft.} \strong{2014}, \emph{59}, DOI 10.18637/jss.v059.i10.
#'  \item H. Wickham, M. Averick, J. Bryan, W. Chang, L. McGowan, R. François, G. Grolemund, A. Hayes, L. Henry, J. Hester, M. Kuhn, T. Pedersen, E. Miller, S. Bache, K. Müller, J. Ooms, D. Robinson, D. Seidel, V. Spinu, K. Takahashi, D. Vaughan, C. Wilke, K. Woo, H. Yutani, \emph{JOSS} \strong{2019}, \emph{4}, 1686, DOI 10.21105/joss.01686.
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
read_featuretable <- function(file, delim = ",", label_col = 1, metadata_cols = NULL, remove_empty_cols = FALSE, show_removed_cols = TRUE, ...) {
  # perform some checks

  # if (!is.null(drop_cols)) {
  #  if (label_col %in% drop_cols) {
  #    rlang::abort("label_col can not be dropped.")
  #  }
  # }


  data <- readr::read_delim(file = file, delim = delim, show_col_types = FALSE, ...)

  if(remove_empty_cols == TRUE) {
    data <- remove_empty_cols(data, always_keep = label_col, show_removed_cols = show_removed_cols)
  }

  convert_from_wide(data, label_col = label_col, metadata_cols = metadata_cols)
}

#' Read a 'full_feature_table' from 'mzmine' into a tidy tibble
#'
#' @description
#' Similar to \code{\link[metamorphr]{read_featuretable}} but specifically for full_feature_table' files created with 'mzmine'. For more information, see \href{https://mzmine.github.io/mzmine_documentation/module_docs/io/feat-list-export.html}{the 'mzmine' documentation}.
#'
#' @param file A path to a file but can also be a connection or literal data.
#' @param intensity A character that specifies what should be used as the (semi-)quantitative measure. Either `"height"` or `"area"`.
#' @param field_separator The field separator as specified in 'mzmine'. Usually `","` if the file is in common CSV format.
#' @param label_col The index or name of the column that will be used to label Features. For example an identifier (_e.g._, KEGG, CAS, HMDB) or a _m/z_-RT pair.
#' @param import_datafile_cols Should columns that begin with `datafile:` be imported? Those columns contain sample-specific information, for example the retention time of a feature measured in a specific sample. Usually, this information is not necessary for downstream analysis but it can be used for quaility control purposes. If `TRUE`, `datafile:` columns are imported and the sample names are removed from the column names. This allows for tidy storage of the information in one column per variable.
#'
#' @return A tidy tibble.
#' @export
#'
#' @references \itemize{
#'  \item H. Wickham, \emph{J. Stat. Soft.} \strong{2014}, \emph{59}, DOI 10.18637/jss.v059.i10.
#'  \item H. Wickham, M. Averick, J. Bryan, W. Chang, L. McGowan, R. François, G. Grolemund, A. Hayes, L. Henry, J. Hester, M. Kuhn, T. Pedersen, E. Miller, S. Bache, K. Müller, J. Ooms, D. Robinson, D. Seidel, V. Spinu, K. Takahashi, D. Vaughan, C. Wilke, K. Woo, H. Yutani, \emph{JOSS} \strong{2019}, \emph{4}, 1686, DOI 10.21105/joss.01686.
#'  \item “12 Tidy data | R for Data Science,” can be found under \url{https://r4ds.had.co.nz/tidy-data.html}, \strong{2023}.
#' }
#'
#' @examples
#' # Read a toy dataset in the format produced with mzmine.
#'
#' featuretable_path <- system.file("extdata", "toy_mzmine.csv", package = "metamorphr")
#'
#' # Example 1: Use feature height as the metric
#' featuretable <- read_featuretable_mzmine(
#'   featuretable_path,
#'   intensity = "height"
#' )
#'
#' featuretable
#'
#' # Example 2: Use feature area as the metric
#' featuretable <- read_featuretable_mzmine(
#'   featuretable_path,
#'   intensity = "area"
#' )
#'
#' featuretable
#'
#' # Example 3: Use the 'mz' column as a Feature label
#' featuretable <- read_featuretable_mzmine(
#'   featuretable_path,
#'   label_col = "mz"
#' )
#'
#' featuretable
read_featuretable_mzmine <- function(file, intensity = "height", field_separator = ",", label_col = 1, import_datafile_cols = FALSE) {
  data <- readr::read_delim(file, delim = field_separator, show_col_types = FALSE)


  file_colnames <- colnames(data)

  if (!(intensity %in% c("height", "area"))) {
    rlang::abort(paste0('Argument `intensity` must be "height" or "area", not "', intensity, '".'))
  }

  #check for duplicate names

  datafile_cols <- stringi::stri_detect_regex(file_colnames, pattern = "^datafile:")

  metadata_colnames <- file_colnames[!datafile_cols]


  intensity_cols <- stringi::stri_detect_regex(file_colnames, pattern = paste0("^datafile:.{1,}", intensity, "$"))

  datafile_cols[intensity_cols] <- FALSE


  datafile_colnames <- file_colnames[datafile_cols]
  intensity_colnames <- file_colnames[intensity_cols]



  if (is.character(label_col)) {
    clone_id <- label_col == "id"
  } else {
    clone_id <- colnames(data)[[label_col]] == "id"
  }


  if(import_datafile_cols == FALSE) {
    data <- dplyr::select(data, -dplyr::all_of(datafile_colnames))
    data <- convert_from_wide(data, label_col = label_col, metadata_cols = metadata_colnames) %>%
      dplyr::mutate(Sample = stringi::stri_replace_all_regex(.data$Sample, "^datafile:", "")) %>%
      dplyr::mutate(Sample = stringi::stri_replace_all_regex(.data$Sample, paste0(":", intensity, "$"), ""))
    return(data)
  } else {
    sample_names_cleaned <- stringi::stri_replace_all_regex(datafile_colnames, "^datafile:", "")
    sample_names_cleaned <- unique(stringi::stri_replace_all_regex(sample_names_cleaned, ":.{1,}$", ""))

    datafile_cols_list <- purrr::map(sample_names_cleaned, function(x, y) {dplyr::select(y, c("id", dplyr::contains(x)))}, y = data)

    datafile_cols_list <- purrr::map(datafile_cols_list, internal_rename_datafile_cols)

    names(datafile_cols_list) <- sample_names_cleaned

    datafile_cols_df <- dplyr::bind_rows(datafile_cols_list, .id = "Sample")

    data <- dplyr::select(data, -dplyr::all_of(datafile_colnames))

    data <- convert_from_wide(data, label_col = label_col, metadata_cols = metadata_colnames)

    if (clone_id == TRUE) {
      data <- dplyr::mutate(data, id = as.numeric(.data$Feature))
    }

    data %>%
      dplyr::mutate(Sample = stringi::stri_replace_all_regex(.data$Sample, "^datafile:", "")) %>%
      dplyr::mutate(Sample = stringi::stri_replace_all_regex(.data$Sample, paste0(":", intensity, "$"), "")) %>%
      dplyr::left_join(datafile_cols_df, by = dplyr::join_by("Sample", "id"))
  }
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
#' \href{https://www.matrixscience.com/help/data_file_help.html}{MGF files} allow the storage of MS/MS spectra. This
#' function reads them into a tidy tibble. Each variable is stored in a column and each ion (observation) is stored in a separate row.
#' MS/MS spectra are stored in a list column named MSn. <br>
#' Please note that \href{https://fiehnlab.ucdavis.edu/projects/lipidblast/mgf-files}{MGF files are software-specific} so the variables
#' and their names may vary. This function was developed with the GNPS file format exported from \href{https://mzio.io/mzmine-news/}{mzmine} in mind.<br><br>
#' If you encounter any bugs please report them: \href{https://github.com/yasche/metamorphr/issues}{https://github.com/yasche/metamorphr/issues}
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
