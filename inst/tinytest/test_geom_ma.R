library(ggplot2)
data(aapl)
p1 <- ggplot(aapl, aes(x = date, y = adjusted)) + 
  geom_line() +
  geom_ma(ma_type = "EMA") 

test_ma_values <- list(ma_type = "EMA",
                       n = 10,
                       wilder = FALSE,
                       ratio = NULL,
                       v = 1,
                       wts = 1:10,
                       na.rm = TRUE)

# check if stat_params are the same as when selecting ma_fun EMA 
expect_equal(p1$layers[[2]]$stat_params, test_ma_values)

# Check if correct EMA calculation has been used in plot
expect_equal(layer_data(p1, i = 2)$y, TTR::EMA(aapl$adjusted, n = 10))

# check on default geom settings.
# Remove this test when package in stable mode
expect_equal(unique(layer_data(p1, i = 2)$colour), "blue")
expect_equal(unique(layer_data(p1, i = 2)$linetype), 1)
expect_equal(unique(layer_data(p1, i = 2)$size), 0.5)
expect_equal(unique(layer_data(p1, i = 2)$alpha), NA)

# Check switching error
expect_error(ggttr:::switch_ma("MAA"), pattern = "Unsupported moving average") 