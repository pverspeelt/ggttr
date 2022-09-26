#' add a Stochastic Oscillator to a financial chart 
#' 
#' `add_stoch()` will add a Stochastic Oscillator subplot below a financial chart. 
#' The default settings are the same as in Interactive Brokers.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param high The high column. Default is "high".
#' @param low The low column. Default is "low".
#' @param fastK Number of periods for fast %K. Default is 14.
#' @param fastD Period of the fast average. Default is 3.
#' @param slowD period of the slow average. Default is 1.
#' @param ma_type Which type of moving average will be used to calculate the Stochastic Oscillator.
#' The default is "EMA". Possible options are: SMA or EMA.
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about the [Stochastic Oscillator](https://www.investopedia.com/terms/s/stochasticoscillator.asp) 
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
#' add_stoch(p, high = "high", low = "low") 
add_stoch <- function(plot,
                    high = "high",
                    low = "low",
                    fastK = 14,
                    fastD = 3,
                    slowD = 1,
                    ma_type = "EMA"
                    ){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #TODO add extra tests
  
  hlc_data <- data.frame(high = plot$data[[high]],
                         low = plot$data[[low]], 
                         close = plot$data[[plot$labels$y]])
  
  
  subplot_data <- data.frame(plot$data[plot$labels$x], TTR::stoch(hlc_data,
                                                          nFastK = fastK,
                                                          nFastD = fastD,
                                                          nSlowD = slowD,
                                                          maType = ma_type))
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  subplot <- ggplot2::ggplot(subplot_data, ggplot2::aes(x = .data[[plot$labels$x]])) + 
    ggplot2::geom_line(ggplot2::aes(y = .data$fastK * 100), colour = "purple", na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$fastD * 100), colour = "darkblue", na.rm = TRUE) + 
    ggplot2::scale_y_continuous(name = "Stoch") +
    ggplot2::annotate("rect", 
                      xmin = min(plot$data[[plot$labels$x]]), 
                      xmax = max(plot$data[[plot$labels$x]]), 
                      ymin = 20, ymax = 80, 
                      fill = "lightblue", alpha = 0.5) + 
    scale_x_finance(dates = plot$data[[plot$labels$x]])
  
  
  plot + subplot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}
