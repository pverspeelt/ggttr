#' add Commodity Channel Index (CCI) to a financial chart 
#' 
#' `add_cci()` will add a Commodity Channel Index (CCI) subplot below a financial chart.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param high The high column. Default is "high".
#' @param low The low column. Default is "low".
#' @param n Number of periods for the CCI calculation. Default is 20.
#' @param ma_type Which type of moving average will be used to calculate the CCI. 
#' The default is "SMA". Possible options are: SMA or EMA.
#' @param c Constant to apply to the mean deviation. Default = 0.015.
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about the [CCI](https://www.investopedia.com/terms/c/commoditychannelindex.asp) 
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
#' add_cci(p, high = "high", low = "low") 
add_cci <- function(plot,
                    high = "high",
                    low = "low",
                    n = 20, 
                    ma_type = "SMA",
                    c = 0.015){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #TODO add extra tests
  
  hlc_data <- data.frame(high = plot$data[[high]],
                         low = plot$data[[low]], 
                         close = plot$data[[plot$labels$y]])
  
  
  plot$data$CCI <- TTR::CCI(hlc_data, 
                            n = n, 
                            maType = ma_type,
                            c = c)
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  subplot <- ggplot2::ggplot(plot$data, ggplot2::aes(x = .data[[plot$labels$x]], y = .data$CCI)) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    ggplot2::annotate("rect", 
                      xmin = min(plot$data[[plot$labels$x]]), 
                      xmax = max(plot$data[[plot$labels$x]]), 
                      ymin = -100, ymax = 100, 
                      fill = "lightblue", alpha = 0.5) +
    scale_x_finance(dates = plot$data[[plot$labels$x]])
  
  plot + subplot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}
