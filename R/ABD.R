#' @title ABD - Age Band Decomposition
#'
#' @description
#' ABD - Age Band Decomposition. This function calculates 
#' standardized tree-ring width chronologies by decomposing 
#' tree-ring width (stdTRW) data into age bands, detrending 
#' each age band separately, and then recombining them to 
#' produce the mean standardized chronology. 
#' Specifically, ABD standardizes each series within its 
#' age band (e.g., each ring width is divided by the corresponding 
#' age-specific expected value from the stdTRW). 
#' Then, standardized values from all age bands are merged and 
#' averaged to produce a composite chronology, preserving both 
#' inter-annual and low-frequency climate signals.
#'
#' @param inTRW
#' tibble. A tibble. The input dataset obtained from 
#' the `TRW_readExcel()` function.
#' 
#' @param min_nTrees_year 
#' Numeric. The minimum number of trees per year required within each age class 
#' to be included in the analysis. 
#' The default is 3. 
#' Using less than three trees may result in poor representation of within-class 
#' variability and is not recommended unless data availability is limited.
#'  
#' @param pct_stdTRW_th Numeric. It defines the threshold for the minimum 
#' proportion of standardized tree-ring width values required within a given 
#' age class. 
#' The default is set to 0.5, 
#' meaning that at least 50% of the possible 
#' values must be present (e.g., 6 out of 10 values for 10-year age bands, 
#' or 11 out of 20 for 20-year bands). 
#' For instance, the final age band of a 94-year-old tree 
#' (i.e., the 91–100-year band) includes only 4 years of growth. 
#' Since this number of years below the threshold, we recommend 
#' excluding that tree from the corresponding age band group.
#'
#' @param pct_Trees_th Numeric. It defines the threshold used to calculate 
#' the mean standardized tree-ring widths within each age band. 
#' The default value is 0.3. 
#' However, when working with small sample sizes (approximately 20 trees or fewer), 
#' it is advisable to increase the threshold to 0.5. 
#' This adjustment helps retain more trees in the analysis while still 
#' accounting for natural growth variability.
#' 
#' @return
#' A tibble with the following columns: year, N_ageBands, ABD, and ABDsd.
#' 
#' @details
#' The function performs age-band decomposition on stdTRW data by filtering out 
#' age classes with insufficient observations (`min_nTrees_year`) or 
#' excessive within-class variation.
#' 
#' The `stdTRW_th` value is particularly important in small datasets: 
#' too strict a threshold may exclude valid data. 
#' If working with approximately 20 trees or fewer, 
#' a threshold of 0.5 is suggested.
#' 
#' Please, see the `Examples` section below for a 
#' demonstration using a typical input tibble obtained with `TRW_readExcel()`.
#' 
#'
#' @family ABD functions
#' 
#' @seealso \code{\link{TRW_readExcel}}, \code{\link{stdTRW}}, \code{\link{plotABD}}
#' 
#' @importFrom stats sd
#' 
#' @export
#' 
#' @examples
#' ABD(inTRW)

ABD <- function(inTRW,
                min_nTrees_year = 3,
                pct_stdTRW_th = .5,
                pct_Trees_th = .3) {
  (n_sampled_trees <- inTRW[[1]]$tree_code |> unique() |> length())
  
  if (min_nTrees_year < 3) {
    warning("
    min_nTrees_year is set to a value < 3. 
    Be cautious: low sample sizes may lead to unrepresentative or unreliable summaries.")
  }
  
  if(n_sampled_trees <= 20 && pct_stdTRW_th < 0.5) {
    warning("Low number of sampled trees detected (\u2264 20).
            Consider using pct_stdTRW_th = 0.5 to avoid excessive data filtering.")
  }
  
  # first dataframe ----
  df_01 <- inTRW[[1]]
  age_class_df <- inTRW[[2]]
  
  suppressMessages(
    df_std_habitat <- df_01 |>
      dplyr::left_join(
        df_01 |>
          dplyr::group_by(tree_code) |>
          dplyr::summarise(meanTRW = mean(TRW, na.rm = T))
      ) |>
      dplyr::mutate(stdTRW = TRW/meanTRW,
                    y_age_code = paste0(year, "_", age_class))
  )
  
  # second dataframe ----
  suppressMessages(
    df_02 <- df_std_habitat |> 
      dplyr::group_by(year, age_class, y_age_code) |>
      dplyr::summarise(N_trees = dplyr::n(),
                       mean_stdTRW_year = mean(stdTRW, na.rm = T)) |>
      dplyr::filter(N_trees >= min_nTrees_year) # 'min_nTrees_year' come argomento della funzione
    # Modificato da nTRW_ageClass a 'min_nTrees_year' = 'numero minimo di alberi per anno'
    
    # 'min_nTrees_year': Numeric. Minimum number of trees required within each age class to be 
    # included in the analysis.
    # The default is 3.
    # Using less than three trees may result in poor representation of within-class variability 
    # and is not recommended unless data availability is limited.
  )
  
  # 1-3 dataframe ----
  suppressMessages(
    df_01_03 <- df_01 |>
      dplyr::left_join(age_class_df) |>
      dplyr::group_by(tree_code, age_class) |>
      dplyr::summarise(nSamples = dplyr::n(),
                       MaxSamples = mean(ageBands, na.rm = T)) |>
      dplyr::mutate(sampRatio = nSamples/MaxSamples) |>
      dplyr::filter(sampRatio > pct_stdTRW_th)
    # 'pct_stdTRW_th': Numeric. A threshold is applied to calculate the mean standardized tree-ring widths 
    # within each age band to ensure sufficient representativeness.
    # The default value is 0.3. However, if the sample size is small (approximately 20 trees or fewer),
    # it is recommended to increase the threshold to 0.5 to retain more trees while accounting 
    # for growth variability.
  )
  
  df_01_03g <- df_01_03 |>
    dplyr::group_by(age_class) |>
    dplyr::summarise(Ntree_age = dplyr::n()) |> 
    dplyr::mutate(eTh = pct_Trees_th * n_sampled_trees) |> 
    dplyr::mutate(age_class_ToBeIncluded = Ntree_age > eTh)
  
  # third dataframe ----
  # Average by age class
  suppressMessages(
    AveAgeClass_df <- df_std_habitat |> 
      dplyr::left_join(df_01_03g) |> 
      dplyr::filter(age_class_ToBeIncluded == TRUE) |> 
      dplyr::group_by(age_class) |> 
      dplyr::summarise(stdTRW_byAgeClass = mean(stdTRW, na.rm = TRUE))
  )
  
  # output ----
  suppressMessages(
    ABD_df <- df_02 |> 
      dplyr::left_join(AveAgeClass_df) |> 
      dplyr::mutate(norm_out = mean_stdTRW_year/stdTRW_byAgeClass) |> 
      dplyr::group_by(year) |>
      dplyr::summarise(N_ageBands = dplyr::n(),
                       ABD = base::mean(norm_out, na.rm = T),
                       ABDsd = stats::sd(norm_out, na.rm = T))
  )
  
  out <- tibble::tibble(
    year = min(df_01$year):max(df_01$year)
    ) |> 
    dplyr::left_join(ABD_df)
    
  return(out)
}
