#' add Aroon Indicator or Aroon Oscillator to a financial chart 
#' 
#' `add_aroon()` will add an Aroon indicator subplot below a financial chart. 
#' The positive direction (aroonUp) is plotted in green, the negative direction (aroonDn) is 
#' plotted in red. If you use the setting `oscillator == TRUE` the Aroon Oscillator will be plotted.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param high The high column. Default is "high".
#' @param low The low column. Default is "low".
#' @param n Number of periods for the Aroon calculation. Default is 20.
#' @param oscillator plot the Aroon Oscillator instead. Default is FALSE and the 
#' Aroon indicator will be plotted.
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about the [Aroon indicator](https://www.investopedia.com/terms/a/aroon.asp) 
#' or the [Aroon Oscillator](https://www.investopedia.com/terms/a/aroonoscillator.asp) 
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
#' add_aroon(p, high = "high", low = "low") 
#' 
#' # plot the aroon oscillator
#' add_aroon(p, high = "high", low = "low", oscillator = TRUE) 
#' 
add_aroon <- function(plot,
                      high = "high",
                      low = "low",
                      n = 20,
                      oscillator = FALSE){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #TODO add extra tests
  
  aroon_data <- data.frame(high = plot$data[[high]],
                           low = plot$data[[low]])
  
  aroon_data <- data.frame(plot$data[plot$labels$x], 
                           TTR::aroon(aroon_data,
                                      n = n))
  if(oscillator == FALSE) {
    aroon_long <- stats::reshape(aroon_data,
                                 direction = "long",
                                 varying = list(c("aroonUp", "aroonDn")),
                                 v.names = "value",
                                 times = c("aroonUp", "aroonDn"),
                                 timevar = "aroon")
  
    aroon_plot <- ggplot2::ggplot(aroon_long, 
                                  ggplot2::aes(x = .data[[plot$labels$x]], 
                                               y = .data$value, 
                                               colour = .data$aroon)) + 
      ggplot2::geom_line(na.rm = TRUE) + 
      ggplot2::scale_y_continuous(name = "Aroon") +
      ggplot2::scale_colour_manual(values = c("aroonUp" = "green",
                                              "aroonDn" = "red"), 
                                   guide = "none") +
      scale_x_finance(dates = plot$data[[plot$labels$x]])
  } else {
    aroon_plot <- ggplot2::ggplot(aroon_data,
                                  ggplot2::aes(x = .data[[plot$labels$x]],
                                               y = .data$oscillator)) +
      ggplot2::geom_line(colour = "darkblue", na.rm = TRUE) + 
      scale_x_finance(dates = plot$data[[plot$labels$x]])
  }

  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  plot + aroon_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}


