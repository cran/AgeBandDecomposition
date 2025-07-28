#' @title stdTRW
#'
#' @description
#' To remove the influences of local site characteristics on tree growth, 
#' this function standardizes the tree-ring width series by dividing each 
#' tree-ring width of a particular series by the mean width of that series. 
#' In dendroclimatic studies, it is advisable to exclude the initial years 
#' or decades of growth (the first “age bands”, i.e., 1-10 and 11-20) as 
#' they are less likely to contain climatic signals rather than the influence 
#' of strong early-year growth competition within the tree stand 
#' (Mazza and Sarris, 2021; Sarris et al., 2007).
#'
#' @param inTRW
#' tibble. The first object resulting from the `TRW_readExcel` function.
#'
#' @return
#' A tibble with the following columns: year, tree_code, TRW, meanTRW, and stdTRW.
#' Together with additional columns of grouping variables based on age class.
#' 
#' @references 
#' Mazza, G., Sarris, D., 2021. Identifying the full spectrum of climatic 
#' signals controlling a tree species' growth and adaptation to climate change. 
#' Ecol. Indic. 130, 108109. https://doi.org/10.1016/j.ecolind.2021.108109.
#' 
#' Sarris, D., Christodoulakis, D., Körner, C., 2007. Recent decline in precipitation 
#' and tree growth in the eastern Mediterranean. 
#' Glob. Chang. Biol. 13 (6), 1187–1200. https://doi.org/10.1111/j.1365-2486.2007.01348.x.
#' 
#' @details
#' 
#' To export a tibble like the one below to an `.xlsx` file, you can use the `writexl` package:
#' ```r
#' mytibble <- stdTRW_df |>
#'   dplyr::group_by(year) |>
#'   dplyr::summarise(
#'     N_trees = dplyr::n(),
#'     mean_stdTRW = mean(stdTRW, na.rm = TRUE)
#'   )
#'
#' writexl::write_xlsx(mytibble, path = "my_stdTRW_df.xlsx")
#' ```
#'
#' This will create an Excel file named `my_stdTRW_df.xlsx` in your 
#' current working directory. Please, check also the **Examples** section 
#' below for how to obtain `stdTRW_df` object.
#' 
#' @family ABD functions
#' @seealso \code{\link{TRW_readExcel}}, \code{\link{ABD}}
#' 
#' @export
#' 
#' @examples
#' inTRW_1 <- inTRW[[1]]
#'
#' stdTRW_df <- stdTRW(inTRW_1)
#'
#' stdTRW_df |>
#' dplyr::group_by(year) |>
#' dplyr::summarise(N_trees = dplyr::n(),
#'        mean_stdTRW = mean(stdTRW, na.rm = TRUE)) |>
#'        ggplot2::ggplot(ggplot2::aes(year, N_trees)) +
#'        ggplot2::geom_line()

stdTRW <- function(inTRW) {
  suppressMessages(
    df_std_habitat <- inTRW |>
      dplyr::left_join(
        inTRW |>
          dplyr::group_by(tree_code) |>
          dplyr::summarise(meanTRW = mean(TRW, na.rm = T))
      ) |>
      dplyr::mutate(stdTRW = TRW/meanTRW,
                    y_age_code = paste0(year, "_", age_class))
  )
  return(df_std_habitat)
}

