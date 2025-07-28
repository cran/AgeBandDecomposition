#' @title plotBAI
#'
#' @description
#' Function for plotting basal area increment (BAI) values derived from `inTRW` object 
#' using the first `tibble` produced by the function `TRW_readExcel`.
#' 
#' @param inTRW
#' tibble. The first object resulting from the `TRW_readExcel` function.
#' 
#' @param linewidth_BAI  numeric. line size for BAI.
#' 
#' @param linewidth_Ntrees numeric. line size for Ntrees.
#' 
#' @param byYears numeric. Spacing (in years) between tick marks on the x-axis of time-related plots.
#' 
#' @param xlim Optional numeric vector of length 2 specifying the limits of the x-axis. 
#' If `NULL` the limits are automatically determined.
#' 
#' @param ylim Optional numeric vector of length 2 specifying the limits of the y-axis 
#' for the ABD panel. If `NULL` the limits are automatically determined.
#' 
#' @param ... Other arguments passed on to methods. Not currently used.
#' 
#' @details
#' This function generates a basic plot displaying the mean chronology 
#' +/- standard error of raw basal area increments (in squared cm) alongside 
#' the corresponding number of trees.
#' 
#'  
#' @return
#' None. A plot is produced.
#' 
#' @family tree ring plotting
#' @seealso \code{\link{plotTRW}},  \code{\link{plotABD}}
#' 
#' @importFrom dplyr lag
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_y_continuous scale_x_continuous sec_axis theme_minimal labs
#' 
#' @export
#' 
#' @examples
#' plotBAI(inTRW
#'     , linewidth_BAI = .5
#'     , linewidth_Ntrees = 1
#'     )

plotBAI <- function(inTRW
                    , linewidth_BAI = 1
                    , linewidth_Ntrees = 1
                    , byYears = 20
                    , xlim = NULL
                    , ylim = NULL
                    , ...) {
  
  df_bai <- inTRW[[1]] |>
    dplyr::select(year, tree_code, TRW) |>
    dplyr::arrange(tree_code, year) |>
    dplyr::mutate(TRW_cumsum_arg = ifelse(!is.na(TRW), TRW, 0)) |> 
    dplyr::group_by(tree_code) |>
    dplyr::mutate(
      TRW_cumsum = cumsum(TRW_cumsum_arg),
      BA = pi * TRW_cumsum^2) |> 
    dplyr::mutate(BA_diff = BA-dplyr::lag(BA)) |>
    dplyr::ungroup() |> 
    dplyr::group_by(year) |>
    dplyr::summarise(Ntrees = dplyr::n(),
                     BAIsd = stats::sd(BA_diff, na.rm = T)/100,
                     BAImean = mean(BA_diff, na.rm = T)/100) |> 
    tidyr::drop_na() |> 
    dplyr::mutate(BAIse = BAIsd/sqrt(Ntrees),
                  BAI_up = BAImean + BAIse,
                  BAI_dw = BAImean - BAIse)
  
  
  max_bai <- max(df_bai$BAImean, na.rm = T)
  max_ntrees_bai <- max(df_bai$Ntrees, na.rm = T)
  scale_factor_bai <- max_bai / max_ntrees_bai
  
  ggplot2::ggplot(df_bai, ggplot2::aes(x = year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = BAI_dw, 
                                      ymax = BAI_up), fill = "grey50", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = BAImean, color = "BAImean"), 
                       linewidth = linewidth_BAI) +
    ggplot2::geom_line(ggplot2::aes(y = Ntrees * scale_factor_bai, color = "Ntrees"),
                       linewidth = linewidth_Ntrees, linetype = "dashed") +
    ggplot2::scale_x_continuous(
      name = "year",
      limits = xlim,
      breaks = seq(
        from = if (!is.null(xlim)) xlim[1] else min(df_bai$year, na.rm = TRUE),
        to   = if (!is.null(xlim)) xlim[2] else max(df_bai$year, na.rm = TRUE),
        by   = byYears)) +
    ggplot2::scale_y_continuous(
      name = expression("mean BAI (cm"^2*")"),
      limits = ylim,
      sec.axis = ggplot2::sec_axis(~ . / scale_factor_bai, name = "N trees")
    ) +
    ggplot2::scale_color_manual(values = c("BAImean" = "darkorange3", 
                                           "Ntrees" = "darkgreen")
                                , labels = c("BAImean" = "mean BAI", "Ntrees" = "N trees")
                                ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "yearly mean BAI and number of trees",
                  x = "year", color = "Legend")
}


