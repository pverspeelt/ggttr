library(ggplot2)
data(aapl)

expect_error(ggplot(aapl, aes(x = date, y = adjusted)) + 
               geom_line() +
               geom_bbs(ma_type = "VMA")
             , pattern = "unsupported moving average") 


# test geom_keltner. Should have same error as geom_bbs
expect_error(ggplot(aapl, aes(x = date, y = adjusted)) + 
                         geom_line() +
                         geom_keltner(ma_type = "VMA")
                       , pattern = "unsupported moving average") 

# test geom_envelope
expect_error(ggplot(aapl, aes(x = date, y = adjusted)) + 
               geom_line() +
               geom_envelope(ma_type = "VMA")
             , pattern = "unsupported moving average") 

expect_error(ggplot(aapl, aes(x = date, y = adjusted)) + 
               geom_line() +
               geom_envelope(p = 105)
             , pattern = "Invalid value of p") 

