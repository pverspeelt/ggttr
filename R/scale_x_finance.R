#' Scale for removing non-business days from ggplot
#' 
#' This scale will ignore weekends and holidays that are present in stock data.
#' 
#' ggplot tends to show missing dates in the plots when plotting data like daily
#' stock data. It adds weekends and other missing days that (most) financial software 
#' will ignore as this is the standard way of plotting financial data.
#'
#' @param dates A column containing the dates
#' @param ... Other arguments passed on to scale_x_continous()
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggttr)
#' 
#' data(aapl)
#' ggplot(aapl, aes(x = date, y = adjusted)) + 
#'   geom_line() + 
#'   scale_x_finance(dates = aapl$date)
scale_x_finance <- function (dates,
                             ...) {
  
  ggplot2::scale_x_continuous(name = NULL,  ..., 
                              trans = my_transformer(dates),
                              labels = ~ as.Date(.x, origin = "1970-01-01"),
                              breaks = get_breaks_date(dates)
  )
}

# transformer function to remove plotting of non-existing dates -----
# see https://stackoverflow.com/questions/73054652/ggplot-new-scale-x-function-failing-to-work-correctly
my_transformer <- function(dates) {
  
  dates <- as.numeric(dates)
  pos   <- seq_along(dates) - 1
  
  transform <- function(x) {
    if(all(is.na(x))) return(x)
    x <- as.numeric(x)
    y <- numeric(length(x))
    in_range <- x >= min(dates) & x <= max(dates)
    y[in_range] <- stats::approx(dates, pos, x[in_range])$y
    y[x < min(dates)] <- x[x < min(dates)] - min(dates)
    y[x > max(dates)] <- x[x > max(dates)] - max(dates) + max(pos)
    y
  }
  
  inverse <- function(x) {
    if(all(is.na(x))) return(x)
    x <- as.numeric(x)
    y <- numeric(length(x))
    in_range <- x >= 0 & x <= max(pos) & !is.na(x)
    y[in_range] <- stats::approx(pos, dates, x[in_range])$y
    y[x < 0] <- x[x < 0] + min(dates)
    y[x > max(pos)] <- max(dates) + x[x > max(pos)] - max(pos)
    y
  }
  
  scales::trans_new(name = "date",
                    transform = transform,
                    inverse   = inverse
  )
}


# calculate which breaks should be shown ----------------------------------
get_breaks_date <- function(x){
  # TODO add calculation when there are more than 24 months. 
  out <- c(1, which(stats::ave(as.numeric(x),format(x,"%Y%m"), FUN = function(x) x == max(x)) == 1))
  x[out]
}

