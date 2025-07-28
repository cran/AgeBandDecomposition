#' @title TRW_readExcel
#'
#' @description
#' This function supports the first step of data import from an Excel file 
#' and arranges the dataset properly for ABD analysis.
#'
#' @param path
#' Path to the xls/xlsx file. The Excel file must be compiled following
#' the instructions of the package at the git-page <https://gitlab.com/Puletti/agebanddecomposition_rpackage>. 
#' See also **Details** here following.
#'
#' @param sheet
#' Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet).
#' Ignored if the sheet is specified via range.
#' If neither argument specifies the sheet, defaults to the first sheet.
#'
#' @param ageBands
#' character. Setting the age band window. It must be set to '1010' if all the
#' age classes have the same size (10 years). It must be '1020' if the age classes
#' have different sizes: 10 years till 100 and then 20 years size.
#'
#' @param limitFirst20y
#' logical. This argument removes the first 20 years from each tree.
#' Default is FALSE (no data filtering).
#' 
#' @param verbose logical. If TRUE, prints additional information during import.
#'
#' @return
#' A list of two objects. The first object is a tibble representing the
#' imported dataset in long format. In this tibble the last two columns are
#' an identification number (id_by_years) and two grouping variables (age_class and ageBands).
#' The second object in the list is a lookup table (tibble), useful for further steps.
#'
#' @details
#' The provided Excel `.xlsx` file contains tree-ring width (TRW) chronologies organized by year.
#' Each column represents an individual tree (e.g., `tree01`, `tree02`, `tree03`, etc.), 
#' with rows corresponding to calendar years.
#' Missing values within the ring-width series (e.g., due to measurement gaps or broken sections) 
#' and the estimated number of rings to the pith 
#' for incomplete cores must be explicitly coded as `-999`.
#'
#' An example `.xlsx` file formatted accordingly is available on the package's GitLab page.
#' 
#' <https://gitlab.com/Puletti/agebanddecomposition_rpackage>
#' 
#' This example can be used to test the functions or as a template for preparing your own data.
#'
#' @family ABD functions
#' @seealso \code{\link{stdTRW}}, \code{\link{ABD}}
#' 
#' @export
#' 
#' @examples
#' # Download the 'ABD_example.xlsx' file from the GitLab page of the package
#' package_gitlab_site <- 'https://gitlab.com/Puletti/agebanddecomposition_rpackage'
#' xls_url <- "/-/raw/main/studio/dati/TRW_example.xlsx"
#' url <- paste0(package_gitlab_site,xls_url)
#' 
#' tmpfile <- tempfile(fileext = ".xlsx")
#' download.file(url,
#'               tmpfile,
#'               mode = "wb")
#' 
#' inTRW_example <- TRW_readExcel(tmpfile, sheet = "Example", ageBands = '1010', limitFirst20y = FALSE)
#' inTRW_example

TRW_readExcel <- function(path, sheet, ageBands, limitFirst20y, verbose = TRUE) {

  inData <- readxl::read_excel(path, sheet)

  df_01all <- inData |>
    tidyr::pivot_longer(
      cols = starts_with("t")
    ) |> dplyr::rename(TRW = value) |> 
    tidyr::drop_na() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(. == -999, NA, .))) |>
    dplyr::rename(tree_code = name) |> 
    dplyr::arrange(tree_code, year) |>
    dplyr::group_by(tree_code) |>
    dplyr::mutate(id_by_years = dplyr::row_number(),
                  age_class1010 = round(dplyr::row_number()/10+.49)) |>
    dplyr::mutate(id_by_years = dplyr::row_number(),
                  age_class1020_o = ifelse(age_class1010-10 <= 0,
                                           age_class1010,
                                           20+round(dplyr::row_number()/20+.49))
    ) |> 
    dplyr::mutate(age_class1020 = match(age_class1020_o, 
                                        sort(unique(age_class1020_o)))) |> 
    dplyr::select(-age_class1020_o)

  age_class1010_df <- tibble::tibble(
    age_class = 1:max(df_01all$age_class1010),
    ageBands = 10
  )

  age_class1020_df <- tibble::tibble(
    age_class = 1:length(unique(df_01all$age_class1020)),
    ageBands = 10) |>
    dplyr::mutate(ageBands = dplyr::case_when(age_class > 10 ~ 20, .default = ageBands))

  if(ageBands == '1010') {
    if (verbose) message('Using 10-10 age band option')
    df_01 <- df_01all |>
      dplyr::select(-age_class1020) |>
      dplyr::rename(age_class = age_class1010) |> 
      dplyr::left_join(age_class1010_df)

    age_class_df <- age_class1010_df
  }

  if(ageBands == '1020') {
    if (verbose) message('Using 10-20 age band option')
    df_01 <- df_01all |>
      dplyr::select(-age_class1010) |>
      dplyr::rename(age_class = age_class1020) |> 
      dplyr::left_join(age_class1020_df)
    
    age_class_df <- age_class1020_df
  }

  if(limitFirst20y == T) {
    df_01 <- df_01 |>
      dplyr::filter(age_class > 2)
  }

  out1 <- list(df_01,age_class_df)

  return(out1)
}
