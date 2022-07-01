#' Plot Moving Averages
#'
#' @inheritParams ggplot2::geom_line
#' @param ma_fun The function used to calculate the moving average. The default
#' is "SMA". Possible options are: SMA, EMA, DEMA, WMA, ZLEMA and HMA. See `?TTR::SMA`
#' for more information.
#' @param n Number of periods to average over. Must be between 1 and `nrow(x)`, inclusive.
#' Defaults to 10.
#' @param wilder logical; if `TRUE`, a Welles Wilder type EMA will be calculated. 
#' Default value is `FALSE`.
#' @param ratio A smoothing/decay ratio. Default is `NULL`.
#' @param v The 'volume factor' (a number between `[0,1]`). Default is 1.
#' @param wts Vector of weights. Length of wts vector must equal the length of x, or n (the default).
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
#'   geom_ma(ma_fun = "EMA")
#' 
geom_ma <- function(mapping = NULL, data = NULL, 
                    position = "identity", 
                    na.rm = TRUE, 
                    show.legend = NA, 
                    inherit.aes = TRUE,
                    ma_fun = "SMA",
                    n = 10, 
                    wilder = FALSE, 
                    ratio = NULL, 
                    v = 1, 
                    wts = 1:n,
                    ...) {
  
  # switch stat functions here so there is one geom call or create specific 
  # geoms for VMA, ALMA and volume weighted MA's
  
  ggplot2::layer(
    stat = stat_ma, data = data, mapping = mapping, geom = geom_ma_line, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ma_fun = ma_fun, n = n, wilder = wilder, ratio = ratio, v = v, wts = wts,
                  na.rm = na.rm, ...)
  )
}

# statistics section -----
stat_ma <- ggplot2::ggproto(
  "stat_ma",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data,
                           scales,
                           ma_fun = "SMA",
                           n = 10,
                           wilder = FALSE,
                           ratio = NULL,
                           v = 1,
                           wts = 1:n) {
    dat <- data.frame(x = data$x)
    dat$y <- switch_ma(
      x = data$y,
      ma_fun = ma_fun,
      n = n,
      wilder = wilder,
      ratio = ratio,
      v = v,
      wts = wts
    )
    dat
  }
)

# utility MA functions -----
# use GeomLine proto and set some defaults, like colour
geom_ma_line <- ggplot2::ggproto(
  "geom_ma_line",
  ggplot2::GeomLine,
  default_aes = ggplot2::aes(
    colour = "blue",
    linetype = 1,
    size = 0.5,
    alpha = NA
  )
)


# switch to the selected MA function
switch_ma <- function(ma_fun, x, n, wilder, ratio, v, wts) {
# TODO add message in this geom to point to the volume and ALMA
  switch(ma_fun,
         SMA = TTR::SMA(x = x, n = n),
         EMA = TTR::EMA(x = x, n = n, wilder = wilder, ratio = ratio),
         DEMA = TTR::DEMA(x = x, n = n, v = v, wilder = wilder, ratio = ratio),
         WMA = TTR::WMA(x = x, n = n, wts = 1:n),
         ZLEMA = TTR::ZLEMA(x = x, n = n, ratio = ratio),
         HMA = TTR::HMA(x = x, n = n),
         stop(sprintf("Unsupported moving average: %s", ma_fun), call. = FALSE)
  )
}

# TODO  
# geom for VMA, ALMA and volume weighted MA's

