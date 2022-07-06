#' Plot a Keltner Channel
#' 
#' The `geom_keltner()` function will plot the Keltner Channel around a security's price. 
#' Unlike Bollinger bands, a Keltner Channel uses the Average True Range to set the channel distance.
#'
#' @inheritParams geom_ma
#' @param ma_type Which type of moving average will be used to calculate the moving 
#' average and the Bollinger Bands. The default is "EMA". Possible options are: SMA or EMA.
#' @param n Number of periods to average over. Defaults to 20.
#' @param atr Average True Range. Default is set to 2.
#' @param wilder logical; if `TRUE`, a Welles Wilder type EMA will be calculated. 
#' Default value is `FALSE`.
#' @param colour_ma,colour_bands Line colours of the Bollinger Bands and the moving average line.
#' @param alpha Transparancy for the Bollinger Band ribbon. 
#' @param fill Fill colour between the outer bands. Default is grey.
#' 
#' @seealso 
#' More information about the [Keltner Channel](https://www.investopedia.com/terms/k/keltnerchannel.asp) 
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
#'   geom_keltner(aes(high = high, low = low, close = close))
#'   
#' # use the SMA instead of the EMA 
#' ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() +
#'   geom_keltner(aes(high = high, low = low, close = close),
#'            ma_type = "SMA")
#'   
geom_keltner <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE,
                     ma_type = "EMA", 
                     n = 20, 
                     atr = 2,
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
  
  
  # Bollinger Bands need either 3 MA lines or 1 MA and one ribbon.
  # calculation wise a ribbon and an Ma is one less TTR::BBands call. 
  ribbon <- ggplot2::layer(
    stat = stat_keltner_ribbon, geom = geom_bb_ribbon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_type = ma_type, n = n, wilder = wilder, atr = atr, 
                  na.rm = na.rm, color = colour_bands, alpha = alpha, 
                  fill = fill, ...)
  )
  
  line <- ggplot2::layer(
    stat = stat_keltner_ma, geom = geom_bbma_line, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_type = ma_type, n = n, wilder = wilder, atr = atr, 
                  na.rm = na.rm, color = colour_ma, alpha = NA, fill = fill, ...)
  )
  
  # Combine the ribbon and MA into 1 return
  list(ribbon, line)
  
}

# statistics section -----
stat_keltner_ribbon <- ggplot2::ggproto("stat_keltner_ribbon", ggplot2::Stat,
                                   required_aes = c("x", "high", "low", "close"),
                                   
                                   compute_group = function(data, scales, params,
                                                            ma_type = ma_type,
                                                            n = n, 
                                                            atr = atr,
                                                            wilder = wilder
                                   ) {
                                     
                                     grid <- data.frame(x = data$x)
                                     
                                     hlc <- data.frame(high  = data$high,
                                                       low   = data$low,
                                                       close = data$close)
                                     
                                     # Switch between SMA and EMA 
                                     if(ma_type == "SMA") {
                                       keltner <- TTR::keltnerChannels(hlc, n = n, maType = ma_type, atr = atr)
                                     } else if(ma_type == "EMA"){
                                       keltner <- TTR::keltnerChannels(hlc, n = n, maType = ma_type, atr = atr, wilder = wilder)
                                     }
                                     
                                     grid$ymin <- keltner[,"dn"]
                                     grid$ymax <- keltner[,"up"]
                                     
                                     grid
                                   }
)


stat_keltner_ma <- ggplot2::ggproto("stat_keltner_ma", 
                               ggplot2::Stat,
                               required_aes = c("x", "high", "low", "close"),
                               
                               compute_group = function(data, scales, params,
                                                        ma_type = ma_type, 
                                                        n = n, 
                                                        wilder = wilder,
                                                        atr = atr,
                                                        fill = "grey") {
                                 
                                 grid <- data.frame(x = data$x)
                                 
                                 hlc <- data.frame(high = data$high,
                                                   low = data$low,
                                                   close = data$close)
                                 
                                 # Switch between SMA and EMA 
                                 if(ma_type == "SMA") {
                                   keltner <- TTR::keltnerChannels(hlc, n = n, maType = ma_type, atr = atr)
                                 } else if(ma_type == "EMA"){
                                   keltner <- TTR::keltnerChannels(hlc, n = n, maType = ma_type, atr = atr, wilder = wilder)
                                 }
                                 
                                 grid$y <- keltner[, "mavg"]
                                 
                                 grid
                               }
)



# utility geom functions -----
# Reuse ribbon and ma colours as defined in geom_bollinger_bands

