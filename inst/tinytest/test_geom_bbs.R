library(ggplot2)
data(aapl)

tinytest::expect_error(ggplot(aapl, aes(x = date, y = adjusted)) + 
               geom_line() +
               geom_bbs(ma_type = "VMA")
             , pattern = "unsupported moving average") 
