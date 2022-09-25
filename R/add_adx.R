#' add Average Directional Index to a financial chart 
#' 
#' `add_adx()` will add an Average Directional Index subplot below a financial chart. 
#' The positive direction (DIp) is plotted in green, the negative direction (DIn) is 
#' plotted in red, and the ADX is plotted in dark blue.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param high The high column. Default is "high".
#' @param low The low column. Default is "low".
#' @param n Number of periods for the ADX calculation. Default is 14.
#' @param ma_type Which type of moving average will be used to calculate the ATR. 
#' The default is "EMA". Possible options are: SMA or EMA.
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about the [ADX](https://www.investopedia.com/terms/a/adx.asp) 
#' indicator can be found on investopedia. 
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
#' add_adx(p, high = "high", low = "low") 
add_adx <- function(plot,
                    high = "high",
                    low = "low",
                    n = 14, 
                    ma_type = "EMA"){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #TODO add extra tests
  
  adx_data <- data.frame(high = plot$data[[high]],
                         low = plot$data[[low]], 
                         close = plot$data[[plot$labels$y]])
  
  adx_data <- cbind(plot$data[plot$labels$x], TTR::ADX(adx_data,
                                                       n = n,
                                                       maType = ma_type))
  
  adx_long <- stats::reshape(adx_data, direction = "long",
                             varying = list(c("DIp", "DIn", "ADX")),
                             v.names = "value",
                             times = c("DIp", "DIn", "ADX"),
                             timevar = "adx")
  
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  adx_plot <- ggplot2::ggplot(adx_long, ggplot2::aes(x = .data[[plot$labels$x]], 
                                                     y = .data$value, 
                                                     colour = .data$adx)) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    ggplot2::scale_y_continuous(name = "ADX") +
    ggplot2::scale_colour_manual(values = c("ADX" = "darkblue",
                                            "DIp" = "green",
                                            "DIn" = "red"), guide = "none") +
    scale_x_finance(dates = plot$data[[plot$labels$x]])

  
  plot + adx_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}
