#' @title TRW_readExcel (Deprecated)
#'
#' @description
#' **This function is deprecated and will be removed in a future version.**
#' Please use \code{\link{import_rwl}} instead after converting your data to RWL format.
#'
#' @param path Path to the xls/xlsx file.
#' @param sheet Sheet to read.
#' @param ageBands character. Age band window setting.
#' @param limitFirst20y logical. Remove first 20 years from each tree.
#' @param verbose logical. Print additional information.
#'
#' @return A list of two objects (see original documentation).
#'
#' @seealso \code{\link{import_rwl}}
#' @export
TRW_readExcel <- function(path, sheet, ageBands, limitFirst20y = FALSE, verbose = TRUE) {
  
  # Deprecation warning
  .Deprecated("import_rwl", 
              package = "AgeBandDecomposition",
              msg = "TRW_readExcel() is deprecated. Please convert your Excel data to RWL format (Tucson standard) and use import_rwl() instead. See ?import_rwl for details.")
  
  # Original function code
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
  
  out1 <- list(df_01, age_class_df)
  return(out1)
}