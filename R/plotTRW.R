#' @title plotTRW
#'
#' @description
#' Function for plotting TRW values from `inTRW` object using the first 
#' `tibble` produced by the function `TRW_readExcel`.
#' 
#' @param inTRW
#' tibble. The first object resulting from the `TRW_readExcel` function.
#' 
#' @param linewidth_TRW numeric. line size for TRW.
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
#' +/- standard error of raw tree-ring widths (in mm) alongside 
#' the corresponding number of trees.
#' 
#' @return
#' None. A plot is produced.
#' 
#' @family tree ring plotting
#' 
#' @seealso \code{\link{plotBAI}}, \code{\link{plotABD}}
#' 
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_y_continuous scale_x_continuous sec_axis theme_minimal labs
#' 
#' @export
#' 
#' @examples
#' 
#' plotTRW(inTRW)
#' 

plotTRW <- function(inTRW
                    , linewidth_TRW = 1
                    , linewidth_Ntrees = 1
                    , byYears = 20
                    , xlim = NULL
                    , ylim = NULL
                    , ...) {
  df <- inTRW[[1]] |>
    dplyr::group_by(year) |>
    dplyr::summarise(Ntrees = dplyr::n(),
                     TRWsd = stats::sd(TRW, na.rm = T),
                     TRWmean = mean(TRW, na.rm = TRUE)) |>
    dplyr::mutate(TRW_label = factor("TRWmean", levels = c("TRWmean", "Ntrees")),
                  Ntrees_label = factor("Ntrees", levels = c("TRWmean", "Ntrees"))) |> 
    dplyr::mutate(TRWse = TRWsd/sqrt(Ntrees),
                  TRW_up = TRWmean + TRWse,
                  TRW_dw = TRWmean - TRWse)
  
  max_trw <- max(df$TRWmean, na.rm = T)
  max_ntrees <- max(df$Ntrees, na.rm = T)
  scale_factor <- max_trw / max_ntrees
  
  
  ggplot2::ggplot(df, ggplot2::aes(x = year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = TRW_dw, 
                             ymax = TRW_up), fill = "grey50", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = TRWmean, color = TRW_label), 
                       linewidth = linewidth_TRW) +
    ggplot2::geom_line(ggplot2::aes(y = Ntrees * scale_factor, color = Ntrees_label),
                       linewidth = linewidth_Ntrees, linetype = "dashed") +
    ggplot2::scale_x_continuous(
      name = "year",
      limits = xlim,
      breaks = seq(
        from = if (!is.null(xlim)) xlim[1] else min(df$year, na.rm = TRUE),
        to   = if (!is.null(xlim)) xlim[2] else max(df$year, na.rm = TRUE),
        by   = byYears)) +
    ggplot2::scale_y_continuous(
      name = "mean TRW (mm)",
      limits = ylim,
      sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = "N trees")
    ) +
    ggplot2::scale_color_manual(
      values = c("TRWmean" = "darkorange3", "Ntrees" = "darkgreen"),
      labels = c("TRWmean" = "mean TRW", "Ntrees" = "N trees")
    )+
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "yearly mean TRW and number of trees",
                  x = "year", color = "Legend")
  }
