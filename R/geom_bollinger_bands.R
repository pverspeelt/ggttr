#' Plot Bollinger Bands
#' 
#' The `geom_bbs()` function will plot the Bollinger Bands around a security's price.
#'
#' @inheritParams geom_ma
#' @param ma_type Which type of moving average will be used to calculate the moving 
#' average and the Bollinger Bands. The default is "SMA". Possible options are: SMA or EMA.
#' @param n Number of periods to average over. Defaults to 20.
#' @param sd The number of standard deviations to use.
#' @param wilder logical; if `TRUE`, a Welles Wilder type EMA will be calculated. 
#' Default value is `FALSE`.
#' @param colour_ma,colour_bands Line colours of the Bollinger Bands and the moving average line.
#' @param alpha Transparancy for the Bollinger Band ribbon. 
#' @param fill Fill colour between the outer bands. Default is grey.
#' 
#' @seealso 
#' More information about [Bollinger Bands](https://www.investopedia.com/terms/b/bollingerbands.asp) 
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
#'   geom_bbs(aes(high = high, low = low, close = close))
#'   
#' # use the EMA instead of the SMA 
#' ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() +
#'   geom_bbs(aes(high = high, low = low, close = close),
#'            ma_type = "EMA")
#'   
geom_bbs <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE,
                     ma_type = "SMA", 
                     n = 20, 
                     sd = 2,
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
    stat = stat_bb_ribbon, geom = geom_bb_ribbon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_type = ma_type, n = n, wilder = wilder, sd = sd, 
                  na.rm = na.rm, color = colour_bands, alpha = alpha, 
                  fill = fill, ...)
  )

  ma <- ggplot2::layer(
    stat = stat_bb_ma, geom = geom_bbma_line, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_type = ma_type, n = n, wilder = wilder, sd = sd, 
                  na.rm = na.rm, color = colour_ma, alpha = NA, fill = fill, ...)
  )

  # Combine the ribbon and MA into 1 return
  list(ribbon, ma)

}

# statistics section -----
stat_bb_ribbon <- ggplot2::ggproto("stat_bb_ribbon", ggplot2::Stat,
                                     required_aes = c("x", "high", "low", "close"),
                                     
                                     compute_group = function(data, scales, params,
                                                              ma_type = "SMA",
                                                              n = 10, 
                                                              sd = 2,
                                                              wilder = FALSE
                                                              ) {
                                       
                                       grid <- data.frame(x = data$x)
                                       
                                       hlc <- data.frame(high  = data$high,
                                                         low   = data$low,
                                                         close = data$close)
                                       
                                       # Switch between SMA and EMA 
                                       if(ma_type == "SMA") {
                                         bbs <- TTR::BBands(hlc, n = n, maType = ma_type, sd = sd)
                                       } else if(ma_type == "EMA"){
                                         bbs <- TTR::BBands(hlc, n = n, maType = ma_type, sd = sd, wilder = wilder)
                                       }
                                       
                                       grid$ymin <- bbs[,"dn"]
                                       grid$ymax <- bbs[,"up"]
                                       
                                       grid
                                     }
)


stat_bb_ma <- ggplot2::ggproto("stat_bb_ma", 
                               ggplot2::Stat,
                               required_aes = c("x", "high", "low", "close"),
                               
                               compute_group = function(data, scales, params,
                                                        ma_type = "SMA", 
                                                        n = 20, 
                                                        wilder = FALSE,
                                                        sd = 2,
                                                        fill = "grey") {
                                 
                                 grid <- data.frame(x = data$x)
                                 
                                 hlc <- data.frame(high = data$high,
                                                   low = data$low,
                                                   close = data$close)
                                  
                                 # Switch between SMA and EMA 
                                 if(ma_type == "SMA") {
                                   bbs <- TTR::BBands(hlc, n = n, maType = ma_type, sd = sd)
                                 } else if(ma_type == "EMA"){
                                   bbs <- TTR::BBands(hlc, n = n, maType = ma_type, sd = sd, wilder = wilder)
                                 }

                                 grid$y <- bbs[, "mavg"]
                                 
                                 grid
                                 }
)



# utility geom functions -----
# use GeomLine proto and set some defaults, like colour

geom_bb_ribbon <- ggplot2::ggproto("geom_bb_ribbon", 
                                   ggplot2::GeomRibbon,
                                   default_aes = ggplot2::aes(colour = "red",
                                                              fill = "grey",
                                                              size = 0.5,
                                                              linetype = 1,
                                                              alpha = 0.2)
)

geom_bbma_line <- ggplot2::ggproto("geom_bbma_line", 
                                   ggplot2::GeomLine,
                                   default_aes = ggplot2::aes(colour = "blue",
                                                              linetype = 1,
                                                              size = 0.5,
                                                              alpha = NA)
)

