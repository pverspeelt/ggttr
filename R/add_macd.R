#' Add MACD to a financial chart
#' 
#' `add_macd()` will add a Moving Average Convergence Divergence indicator sub plot below a financial chart.
#'
#' @param plot Needs to be a ggplot2 plot containing stock
#' @param ma_type Which type of moving average will be used to calculate the macd. 
#' The default is "EMA". Possible options are: SMA or EMA.
#' @param fast Number of periods for fast moving average.
#' @param slow Number of periods for slow moving average.
#' @param signal Number of periods for signal moving average.
#' @param percent logical; if TRUE, the percentage difference between the fast 
#' and slow moving averages is returned, otherwise the difference between the 
#' respective averages is returned.
#'
#' @return Plots a patchwork object.  
#' @export
#' 
#' @seealso 
#' More information about [MACD](https://www.investopedia.com/terms/m/macd.asp)
#' can be found on investopedia.
#'
#' @examples
#' library(ggplot2)
#' library(ggttr)
#' 
#' data(aapl)
#' 
#' p <- ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() +
#'   scale_x_finance(dates = aapl$date)
#'   
#' add_macd(p) 
add_macd <- function(plot, 
                     # close = "close",
                     ma_type = "EMA", 
                     fast = 12,
                     slow = 26,
                     signal = 9,
                     percent = TRUE){
  
  # move function out of function for reuse?
  # create separate macd plot for single use plotting?
  get_macd <- function(x, 
                       close = "close",
                       ma_type = ma_type,
                       fast = fast,
                       slow = slow, 
                       signal = signal,
                       percent = percent){
    
    input = x[, grep(close, colnames(x), ignore.case = TRUE)]
    # print(head(input))
    out <- cbind(x, data.frame(TTR::MACD(input, 
                                         maType = ma_type,
                                         nFast = fast, 
                                         nSlow = slow, 
                                         nSig = signal, 
                                         percent = percent)))
    out$hist <- out[, "macd"] - out[, "signal"]
    out
  }
  

  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  macd <- get_macd(plot$data,
                   close = plot$labels$y,
                   ma_type = ma_type,
                   fast = fast,
                   slow = slow, 
                   signal = signal,
                   percent = percent)
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())

  macd$colour <- ifelse(macd$hist < 0, "darkred", "seagreen")
  
  macd_plot <- ggplot2::ggplot(macd, ggplot2::aes(x = .data[[plot$labels$x]])) +
    #na.rm = TRUE doesn't work correctly with geom_col. 
    # See https://github.com/tidyverse/ggplot2/issues/3532
    ggplot2::geom_col(data = stats::na.omit(macd), ggplot2::aes(y = .data$hist, fill = .data$colour)) +
    ggplot2::scale_fill_manual(values = unique(stats::na.omit(macd$colour)), guide = "none") +
    ggplot2::geom_line(ggplot2::aes(y = .data$macd), na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$signal), colour = "red", linetype = 2, na.rm = TRUE) +
    scale_x_finance(dates = macd[[plot$labels$x]]) + 
    ggplot2::ylab("macd")

  plot + macd_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))

}
