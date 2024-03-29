% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_aroon.R
\name{add_aroon}
\alias{add_aroon}
\title{add Aroon Indicator or Aroon Oscillator to a financial chart}
\usage{
add_aroon(plot, high = "high", low = "low", n = 20, oscillator = FALSE)
}
\arguments{
\item{plot}{Needs to be a ggplot2 financial plot containing stock prices.}

\item{high}{The high column. Default is "high".}

\item{low}{The low column. Default is "low".}

\item{n}{Number of periods for the Aroon calculation. Default is 20.}

\item{oscillator}{plot the Aroon Oscillator instead. Default is FALSE and the
Aroon indicator will be plotted.}
}
\value{
Plots a patchwork object.
}
\description{
\code{add_aroon()} will add an Aroon indicator subplot below a financial chart.
The positive direction (aroonUp) is plotted in green, the negative direction (aroonDn) is
plotted in red. If you use the setting \code{oscillator == TRUE} the Aroon Oscillator will be plotted.
}
\examples{
library(ggplot2)
library(ggttr)

data(aapl)

p <- ggplot(aapl, aes(x = date, y = adjusted)) + 
  geom_line() +
  scale_x_finance(dates = aapl$date)
  
add_aroon(p, high = "high", low = "low") 

# plot the aroon oscillator
add_aroon(p, high = "high", low = "low", oscillator = TRUE) 

}
\seealso{
More information about the \href{https://www.investopedia.com/terms/a/aroon.asp}{Aroon indicator}
or the \href{https://www.investopedia.com/terms/a/aroonoscillator.asp}{Aroon Oscillator}
can be found on investopedia.
}
