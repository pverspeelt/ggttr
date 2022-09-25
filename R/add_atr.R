#' add Average True Range indicator to a financial chart 
#' 
#' `add_atr()` will add an Average True Range subplot below a financial chart.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param high The high column. Default is "high".
#' @param low The low column. Default is "low".
#' @param n Number of periods for the ATR calculation. Default is 14.
#' @param ma_type Which type of moving average will be used to calculate the ATR. 
#' The default is "EMA". Possible options are: SMA or EMA.
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about the [ATR](https://www.investopedia.com/terms/a/atr.asp) 
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
#' add_atr(p, high = "high", low = "low") 
add_atr <- function(plot,
                    high = "high",
                    low = "low",
                    n = 14, 
                    ma_type = "EMA"){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #TODO add extra tests
  
  atr_data <- data.frame(high = plot$data[[high]],
                         low = plot$data[[low]], 
                         close = plot$data[[plot$labels$y]])
  
  plot$data$atr <- data.frame(TTR::ATR(atr_data, 
                                       n = n, 
                                       maType = ma_type))$atr
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  atr_plot <- ggplot2::ggplot(plot$data, ggplot2::aes(x = .data[[plot$labels$x]], y = .data$atr)) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    scale_x_finance(dates = plot$data[[plot$labels$x]])
  
  plot + atr_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}

