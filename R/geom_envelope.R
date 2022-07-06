#' Plot a moving average envelope
#' 
#' The `geom_envelope()` function will plot a moving average envelope around a security's price. 
#' Unlike Bollinger bands, a moving average envelope uses a percentage to set the channel distance.
#'
#' @inheritParams geom_ma
#' @param ma_type Which type of moving average will be used to calculate the moving 
#' average and the Bollinger Bands. The default is "SMA". Possible options are: SMA or EMA.
#' @param n Number of periods to average over. Defaults to 20.
#' @param p percentage to calculate the distance. Default is set to 2.5%. Values of p 
#' can be between 0 and 100.
#' @param wilder logical; if `TRUE`, a Welles Wilder type EMA will be calculated. 
#' Default value is `FALSE`.
#' @param colour_ma,colour_bands Line colours of the Bollinger Bands and the moving average line.
#' @param alpha Transparancy for the Bollinger Band ribbon. 
#' @param fill Fill colour between the outer bands. Default is grey.
#' 
#' @seealso 
#' More information about [envelope channels](https://www.investopedia.com/terms/e/envelope-channel.asp) 
#' can be found on investopedia.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggttr)
#' 
#' data(aapl)
#' 
#' ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() +
#'   geom_envelope()
#'   
#' # use the EMA instead of the SMA 
#' ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() +
#'   geom_envelope(ma_type = "EMA", p = 2)
#'   
geom_envelope <- function(mapping = NULL, data = NULL,
                         position = "identity", na.rm = TRUE, show.legend = NA,
                         inherit.aes = TRUE,
                         ma_type = "SMA", 
                         n = 20, 
                         p = 2.5,
                         wilder = FALSE, 
                         colour_ma = "blue", 
                         colour_bands = "red",
                         alpha = 0.2, 
                         fill = "grey", 
                         ...) {
  
  
  if(!ma_type %in% c("SMA", "EMA")){
    stop(sprintf("You suplied an unsupported moving average: %s. \nIf you want to use a different MA, please submit a github issue.", 
                 ma_type), call. = FALSE)
  }
  
  if(p > 100 || p < 0){
    stop(sprintf("Invalid value of p. p needs to be between 0 and 100. \nYour value of p is %d", p),
         call. = FALSE)
  }
  
  
  # Bollinger Bands need either 3 MA lines or 1 MA and one ribbon.
  # calculation wise a ribbon and an Ma is one less TTR::BBands call. 
  ribbon <- ggplot2::layer(
    stat = stat_envelope_ribbon, geom = geom_bb_ribbon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_type = ma_type, n = n, wilder = wilder, p = p, 
                  na.rm = na.rm, color = colour_bands, alpha = alpha, 
                  fill = fill, ...)
  )
  
  line <- ggplot2::layer(
    stat = stat_envelope_ma, geom = geom_bbma_line, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_type = ma_type, n = n, wilder = wilder, p = p, 
                  na.rm = na.rm, color = colour_ma, alpha = NA, fill = fill, ...)
  )
  
  # Combine the ribbon and MA into 1 return
  list(ribbon, line)
  
}

# statistics section -----
stat_envelope_ribbon <- ggplot2::ggproto("stat_envelope_ribbon", ggplot2::Stat,
                                        required_aes = c("x", "y"),
                                        
                                        compute_group = function(data, scales, params,
                                                                 ma_type = ma_type,
                                                                 n = n, 
                                                                 p = p,
                                                                 wilder = wilder
                                        ) {
                                          
                                          grid <- data.frame(x = data$x)
                                          

                                          # Switch between SMA and EMA 
                                          if (ma_type == "SMA") {
                                            movavg <- TTR::SMA(x = data$y, n = n)
                                          } else {
                                            movavg <- TTR::EMA(x = data$y, n = n, wilder = wilder)
                                          }
                                          
                                          grid$ymin <- movavg * (1 - p/100)
                                          grid$ymax <- movavg * (1 + p/100)
                                          
                                          grid
                                        }
)


stat_envelope_ma <- ggplot2::ggproto("stat_envelope_ma", 
                                    ggplot2::Stat,
                                    required_aes = c("x", "y"),
                                    
                                    compute_group = function(data, scales, params,
                                                             ma_type = ma_type, 
                                                             n = n, 
                                                             wilder = wilder,
                                                             p = p,
                                                             fill = "grey") {
                                      
                                      grid <- data.frame(x = data$x)
                                      
                                      # Switch between SMA and EMA 
                                      if (ma_type == "SMA") {
                                        movavg <- TTR::SMA(x = data$y, n = n)
                                      } else {
                                        movavg <- TTR::EMA(x = data$y, n = n, wilder = wilder)
                                      }
                                      
                                      grid$y <- movavg
                                      
                                      grid
                                    }
)



# utility geom functions -----
# Reuse ribbon and ma colours as defined in geom_bollinger_bands

