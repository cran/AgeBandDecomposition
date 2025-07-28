#' @title plotABD
#'
#' @description
#' Function for plotting ABD values from `inTRW` object using the two `tibble` objects 
#' produced by the function `TRW_readExcel`.
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
#' @param xlim Optional numeric vector of length 2 specifying the limits of the x-axis. 
#' If `NULL` the limits are automatically determined.
#' 
#' @param ylimABD Optional numeric vector of length 2 specifying the limits of the y-axis 
#' for the ABD panel. If `NULL` the limits are automatically determined.
#' 
#' @param linewidth_TRW numeric. line size for TRW.
#' 
#' @param linewidth_Ntrees numeric. line size for Ntrees.
#' 
#' @param byYears numeric. Spacing (in years) between tick marks on the x-axis of time-related plots.
#' 
#' @param ... Other arguments passed on to methods. Not currently used.
#' 
#' @details
#' This function produces a multi-panel plot with three graphs: 
#' the first displays standardized values by age band; 
#' the second shows the final mean chronology (ABD), adjusted for age-related effects; 
#' and the third depicts the corresponding number of trees.
#' 
#' 
#' @return
#' None. A multipanel plot.
#' 
#' @family tree ring plotting
#' 
#' @seealso \code{\link{plotBAI}}, \code{\link{plotTRW}},  \code{\link{ABD}}
#' 
#' @importFrom ggplot2 aes geom_line facet_wrap theme_minimal theme ylab
#' 
#' @export
#' 
#' @examples
#' 
#' plotABD(inTRW, 
#'    min_nTrees_year = 3, 
#'    pct_stdTRW_th = .5,
#'    pct_Trees_th = .3,
#'    byYears = 50)

plotABD <- function(inTRW
                    , min_nTrees_year = 3
                    , pct_stdTRW_th = .5
                    , pct_Trees_th = .3
                    , linewidth_TRW = .7
                    , linewidth_Ntrees = 1
                    , byYears = 20
                    , xlim = NULL
                    , ylimABD = NULL
                    , ...) {
  
  (n_sampled_trees <- inTRW[[1]]$tree_code |> unique() |> length())
  
  # first dataframe ----
  df_01 <- inTRW[[1]]
  age_class_df <- inTRW[[2]]
  
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
      dplyr::filter(N_trees >= min_nTrees_year)
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
  
  # subplot 1 ----
  plot1 <- df_02 |> 
    dplyr::left_join(
      age_class_df |> 
        dplyr::mutate(
          start_year = (dplyr::lag(cumsum(ageBands), default = 0) + 1),
          end_year = cumsum(ageBands),
          year_range = paste0(start_year, "-", end_year),
          year_range = factor(year_range, levels = year_range, ordered = TRUE)
        )  |> 
        dplyr::select(-start_year, -end_year)
    ) |> 
    dplyr::left_join(df_01_03g) |> 
    dplyr::filter(age_class_ToBeIncluded==TRUE) |> 
    ggplot2::ggplot(ggplot2::aes(x = year, y = mean_stdTRW_year)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~year_range, ncol = 1, strip.position = "left") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y           = ggplot2::element_blank(),
      axis.ticks.y          = ggplot2::element_blank(),
      axis.title.y          = ggplot2::element_blank(),
      panel.spacing         = ggplot2::unit(0.5, "lines"),
      strip.text.y.left    = ggplot2::element_text(angle = 0, hjust = 1)
      , strip.text.y.right = ggplot2::element_text(angle = 0)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(min(df_01$year, na.rm = TRUE),
                                             max(df_01$year, na.rm = TRUE), 
                                             by = byYears)) +
    ggplot2::ylab("TRW - ageBands") +
    (if (!is.null(xlim)) ggplot2::coord_cartesian(xlim = xlim) else ggplot2::coord_cartesian())
  
  # subplot 2 ----
  ABD_df <- ABD(inTRW, 
                min_nTrees_year,
                pct_stdTRW_th,
                pct_Trees_th) |> 
    dplyr::mutate(ABDse = ABDsd/sqrt(N_ageBands),
                  ABD_up = ABD + ABDse,
                  ABD_dw = ABD - ABDse)
  
  
  plot2 <- ABD_df |>
    ggplot2::ggplot(ggplot2::aes(year, ABD)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ABD_dw, ymax = ABD_up),
      fill = "grey50", alpha = 0.5
    ) +
    ggplot2::geom_line(color = "black", linewidth = linewidth_TRW) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("ABD") +
    ggplot2::coord_cartesian(xlim = xlim) +  
    (if (!is.null(ylimABD)) ggplot2::ylim(ylimABD) else NULL) +
    ggplot2::scale_x_continuous(breaks = seq(min(df_01$year, na.rm = TRUE),
                                             max(df_01$year, na.rm = TRUE), 
                                             by = byYears))
  
  
  # subplot 3 ----
  df <- inTRW[[1]] |>
    dplyr::group_by(year) |>
    dplyr::summarise(Ntrees = dplyr::n())
  
  plot3 <- ggplot2::ggplot(df, ggplot2::aes(x = year)) +
    ggplot2::geom_line(
      ggplot2::aes(y = Ntrees),
      linewidth = linewidth_Ntrees,
      linetype = "dashed",
      color = "darkorange3"
    ) +
    ggplot2::ylab("N trees") +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::scale_x_continuous(breaks = seq(min(df_01$year, na.rm = TRUE),
                                             max(df_01$year, na.rm = TRUE), 
                                             by = byYears))
  
  # fusione ----
  p <- (plot1 | patchwork::wrap_plots(plot2, plot3, ncol = 1, heights = c(1, 2))) +
    patchwork::plot_layout(widths = c(2, 3))
  
  return(p)
}
