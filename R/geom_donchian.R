#' Plot a Donchian Channel
#' 
#' The `geom_donchian()` function will plot the Donchian channel around a security's price.
#' 
#' @inheritParams geom_bbs
#' @param include_lag Should values be lagged so that today's prices are not included in the calculation?
#'
#' @export
#' @seealso 
#' More information about [Donchian channels](https://www.investopedia.com/terms/d/donchianchannels.asp) 
#' can be found on investopedia.
#'
#' @examples
#' library(ggplot2)
#' library(ggttr)
#' 
#' data(aapl)
#' 
#' ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() +
#'   geom_donchian(aes(high = high, low = low))
#'   
geom_donchian <- function(mapping = NULL, data = NULL,
                         position = "identity", na.rm = TRUE, show.legend = NA,
                         inherit.aes = TRUE,
                         n = 20, 
                         include_lag = FALSE, 
                         colour_ma = "blue", 
                         colour_bands = "red",
                         alpha = 0.2, 
                         fill = "grey", 
                         ...) {
  
  # Reuse geom_bb_ribbon and geom_bbma_line colours as defined in geom_bollinger_bands
  
  ribbon <- ggplot2::layer(
    stat = stat_donch_ribbon, geom = geom_bb_ribbon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, include_lag = include_lag,
                  na.rm = na.rm, color = colour_bands, alpha = alpha, 
                  fill = fill, ...)
  )
  
  line <- ggplot2::layer(
    stat = stat_donch_line, geom = geom_bbma_line, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, include_lag = include_lag, na.rm = na.rm,
                  color = colour_ma, alpha = NA, fill = fill, ...)
  )
  
  # Combine the ribbon and MA into 1 return
  list(ribbon, line)
  
}

# statistics section -----
stat_donch_ribbon <- ggplot2::ggproto("stat_donch_ribbon", 
                                      ggplot2::Stat,
                                      required_aes = c("x", "high", "low"),
                                   
                                   compute_group = function(data, 
                                                            scales, 
                                                            params,
                                                            n = n,
                                                            include_lag = include_lag
                                                            ) {
                                     
                                     grid <- data.frame(x = data$x)
                                     
                                     hl <- data.frame(high  = data$high,
                                                      low   = data$low)
                                     
                                     donchian <- TTR::DonchianChannel(hl, 
                                                                      n = n, 
                                                                      include.lag = include_lag)
                                     
                                     grid$ymin <- donchian[,"low"]
                                     grid$ymax <- donchian[,"high"]
                                     
                                     grid
                                   }
)

stat_donch_line <- ggplot2::ggproto("stat_donch_line",
                               ggplot2::Stat,
                               required_aes = c("x", "high", "low"),
  
  compute_group = function(data,
                           scales,
                           params,
                           n = n,
                           include_lag = include_lag,
                           fill = "grey") {
    grid <- data.frame(x = data$x)
    
    hl <- data.frame(high = data$high,
                     low = data$low)
    
    donchian <- TTR::DonchianChannel(hl, n = n, include.lag = include_lag)
    
    grid$y <- donchian[, "mid"]
    
    grid
  }
)



# utility geom functions -----
# Reuse ribbon and ma colours as defined in geom_bollinger_bands
