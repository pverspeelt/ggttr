#' Add RSI to a financial chart
#'
#' @param plot Needs to be a ggplot2 financial plot containing stock prices.
#' @param n Number of periods for the RSI calculation. Default is 14.
#'
#' @return Plots a patchwork object.
#' @export
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
#' add_rsi(p) 
add_rsi <- function(plot,
                    n = 14){
  
  if(inherits(plot, "patchwork")) 
    stop(sprintf("The plot you supplied is a patchwork object"), call. = FALSE)
  
  if(!inherits(plot, "ggplot"))
    stop(sprintf("Please supply a ggplot object"), call. = FALSE)
  
  #rsi based on ma_type = "EMA" and wilder = TRUe
  plot$data$rsi <- TTR::RSI(price = plot$data[[plot$labels$y]],
                            n = n)
  
  plot <- plot + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  
  rsi_plot <- ggplot2::ggplot(plot$data, ggplot2::aes(x = .data[[plot$labels$x]], y = rsi)) + 
    ggplot2::geom_line(na.rm = TRUE) + 
    ggplot2::geom_hline(yintercept = 30, linetype = 2, color = "darkseagreen") +
    ggplot2::geom_hline(yintercept = 70, linetype = 2, color = "orangered") +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
    scale_x_finance(dates = plot$data[[plot$labels$x]])
  
  plot + rsi_plot + patchwork::plot_layout(nrow = 2, byrow = FALSE, heights = c(3, 1))
  
}