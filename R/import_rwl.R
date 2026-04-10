#' @title import_rwl
#'
#' @description
#' This function imports tree-ring width data from RWL (Tucson format) files
#' and pith offset information, then arranges the dataset for ABD analysis.
#'
#' @param rwl_path
#' Path to the RWL file (Tucson format).
#'
#' @param po_path
#' Path to the pith offset file (tab-delimited text file with columns 
#' 'tree_code' and 'pith.offset').
#'
#' @param ageBands
#' character. Setting the age band window. It must be set to '1010' if all the
#' age classes have the same size (10 years). It must be '1020' if the age classes
#' have different sizes: 10 years till 100 and then 20 years size.
#'
#' @param first_age_class
#' numeric. is numeric and specifies the first age band from which the analysis begins. 
#' If NULL (default),
#' no filtering is applied. For example, first_age_class = 3 excludes the first
#' 20 years of growth (age classes 1 and 2 in '1010' mode).
#'
#' @param zero_as_na
#' logical. If TRUE (default), zero values in TRW are converted to NA.
#' If FALSE, zero values are kept as is.
#'
#' @param verbose 
#' logical. If TRUE, prints additional information during import.
#'
#' @return
#' A list of two objects. The first object is a tibble representing the
#' imported dataset in long format. In this tibble the last two columns are
#' an identification number (id_by_years) and two grouping variables (age_class and ageBands).
#' The second object in the list is a lookup table (tibble), useful for further steps.
#'
#' @details
#' The RWL file must be in Tucson format (readable by dplR::read.rwl).
#' The pith offset file must be a tab-delimited text file with header containing
#' columns 'tree_code' and 'pith.offset', where pith.offset indicates the number
#' of missing rings between the pith and the first measured ring.
#'  are available on the package's GitLab page.
#'  <https://gitlab.com/Puletti/agebanddecomposition_rpackage>
#' and can be used to test the package's functions.
#' @importFrom utils capture.output read.table
#' @family ABD functions
#' @seealso \code{\link{stdTRW}}, \code{\link{ABD}}
#' @examples
#' # Download example files from the package's GitLab page
#' package_gitlab_site <- 'https://gitlab.com/Puletti/agebanddecomposition_rpackage'
#' rwl_url <- "/-/raw/main/studio/dati/TRW_example.rwl"
#' po_url <- "/-/raw/main/studio/dati/pith.offset.txt"
#' 
#' # Create temporary files
#' tmpfile_rwl <- tempfile(fileext = ".rwl")
#' tmpfile_po <- tempfile(fileext = ".txt")
#' 
#' # Download the files
#' download.file(paste0(package_gitlab_site, rwl_url),
#'               tmpfile_rwl,
#'               mode = "wb")
#' 
#' download.file(paste0(package_gitlab_site, po_url),
#'               tmpfile_po,
#'               mode = "wb")
#'               
#' # View the two files in console
#' dplR::read.rwl(tmpfile_rwl)
#' read.table(tmpfile_po, header = TRUE, dec = ".")
#' 
#' # The same (internal) dataset can be viewed by the following command
#' rwl_test_data; po_test_data
#' 
#' # Import data
#' inData <- import_rwl(rwl_path = tmpfile_rwl, 
#'                              po_path = tmpfile_po, 
#'                              ageBands = '1010', 
#'                              first_age_class = 3,
#'                              zero_as_na = TRUE,
#'                              verbose = TRUE)
#' 
#' # View the result
#' inData
#' @export
import_rwl <- function(rwl_path, po_path, ageBands,
                       first_age_class = NULL, # New parameter
                       zero_as_na = TRUE, verbose = TRUE) {
  
  # Check if files exist
  if(!file.exists(rwl_path)) {
    stop("RWL file not found at: ", rwl_path)
  }
  
  if(!file.exists(po_path)) {
    stop("Pith offset file not found at: ", po_path)
  }
  
  # 1) Read the RWL file (suppressing dplR output based on verbose setting)
  invisible(capture.output(dat.rwl <- dplR::read.rwl(rwl_path)))
  
  # 2) Read the pith offset file
  dat.po <- read.table(po_path, header = TRUE, dec = ".")
  
  # Rename 'tree' column to 'tree_code' if necessary
  if("tree" %in% names(dat.po) && !"tree_code" %in% names(dat.po)) {
    dat.po <- dat.po |> dplyr::rename(tree_code = tree)
  }
  
  # 3) Convert RWL to long format, removing structural NAs
  rwl_long <- dat.rwl |> 
    tibble::rownames_to_column("year") |>
    tidyr::pivot_longer(cols = -year, 
                        names_to = "tree_code", 
                        values_to = "TRW") |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::filter(!is.na(TRW))
  
  # Convert zeros to NA if requested
  if(zero_as_na) {
    rwl_long <- rwl_long |>
      dplyr::mutate(TRW = ifelse(TRW == 0, NA_real_, TRW))
  }
  
  # 4) Add pith offset as NA values for each tree
  rwl_complete <- rwl_long |>
    dplyr::group_by(tree_code) |>
    dplyr::arrange(year) |>
    dplyr::summarise(
      first_year = min(year[!is.na(TRW)], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(dat.po, by = "tree_code") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      offset_data = list(
        if(pith.offset > 0) {
          tibble::tibble(
            year = (first_year - pith.offset):(first_year - 1),
            TRW = NA_real_
          )
        } else {
          tibble::tibble(year = integer(), TRW = numeric())
        }
      )
    ) |>
    dplyr::select(tree_code, offset_data) |>
    tidyr::unnest(offset_data) |>
    dplyr::bind_rows(rwl_long) |>
    dplyr::arrange(tree_code, year)
  
  # 5) Add age_class columns following the original function logic
  df_01all <- rwl_complete |>
    dplyr::group_by(tree_code) |>
    dplyr::mutate(
      id_by_years = dplyr::row_number(),
      age_class1010 = round(dplyr::row_number()/10 + 0.49)
    ) |>
    dplyr::mutate(
      age_class1020_o = ifelse(age_class1010 - 10 <= 0,
                               age_class1010,
                               20 + round(dplyr::row_number()/20 + 0.49))
    ) |>
    dplyr::mutate(
      age_class1020 = match(age_class1020_o, sort(unique(age_class1020_o)))
    ) |>
    dplyr::select(-age_class1020_o)
  
  # 6) Create lookup tables for age_class
  age_class1010_df <- tibble::tibble(
    age_class = 1:max(df_01all$age_class1010),
    ageBands = 10
  )
  
  age_class1020_df <- tibble::tibble(
    age_class = 1:length(unique(df_01all$age_class1020)),
    ageBands = 10
  ) |>
    dplyr::mutate(ageBands = dplyr::case_when(age_class > 10 ~ 20, .default = ageBands))
  
  # 7) Select the appropriate ageBands option
  if(ageBands == '1010') {
    if (verbose) message('Using 10-10 age band option')
    df_01 <- df_01all |>
      dplyr::select(-age_class1020) |>
      dplyr::rename(age_class = age_class1010) |>
      dplyr::left_join(age_class1010_df, by = "age_class")
    age_class_df <- age_class1010_df
  }
  
  if(ageBands == '1020') {
    if (verbose) message('Using 10-20 age band option')
    df_01 <- df_01all |>
      dplyr::select(-age_class1010) |>
      dplyr::rename(age_class = age_class1020) |>
      dplyr::left_join(age_class1020_df, by = "age_class")
    age_class_df <- age_class1020_df
  }
  
  # 8) Filter by minimum age class if requested
  if(!is.null(first_age_class)) {
    if(verbose) message(paste0('Filtering to age_class >= ', first_age_class))
    df_01 <- df_01 |>
      dplyr::filter(age_class >= first_age_class)
  }
  
  # 9) Return output as list
  out1 <- list(df_01, age_class_df)
  return(out1)
}
