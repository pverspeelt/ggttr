#' Add a Momentum indicator to a financial chart
#' 
#' The momentum indicator plots the difference between 2 observations.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param n Number of periods to use. Default is 1.
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about [momentum](https://www.investopedia.com/terms/m/momentum.asp) 
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
#' add_momentum(p) 
add_momentum <- function(plot,
                         n = 1){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #rsi based on ma_type = "EMA" and wilder = TRUe
  plot$data$momentum <- TTR::momentum(x = plot$data[[plot$labels$y]],
                                      n = n)
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  momentum_plot <- ggplot2::ggplot(plot$data, ggplot2::aes(x = .data[[plot$labels$x]], y = .data$momentum)) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    scale_x_finance(dates = plot$data[[plot$labels$x]])
  
  plot + momentum_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}


#' Add a ROC indicator plot to a financial chart
#' 
#' The Rate of Change indicator plots the percentage difference between 2 observations.
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param n Number of periods to use. Default is 1.
#' @param type Compounding type; either "continuous" (the default) or "discrete".
#'
#' @return Plots a patchwork object.
#' @export
#' 
#' @seealso 
#' More information about [ROC](https://www.investopedia.com/terms/p/pricerateofchange.asp) 
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
#' add_roc(p) 
add_roc <- function(plot,
                    n = 1,
                    type = c("continuous")){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #rsi based on ma_type = "EMA" and wilder = TRUe
  plot$data$roc <- TTR::ROC(x = plot$data[[plot$labels$y]],
                            n = n,
                            type = type)
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  roc_plot <- ggplot2::ggplot(plot$data, ggplot2::aes(x = .data[[plot$labels$x]], y = .data$roc)) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    scale_x_finance(dates = plot$data[[plot$labels$x]])
  
  plot + roc_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}
