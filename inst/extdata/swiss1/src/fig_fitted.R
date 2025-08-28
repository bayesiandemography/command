
suppressPackageStartupMessages({
  library(ggplot2)
  library(command)
})

cmd_assign(.vals_fitted = "out/vals_fitted.rds",
           .out = "out/fig_fitted.png")

vals_fitted <- readRDS(.vals_fitted)

p <- ggplot(vals_fitted, aes(x = agriculture)) +
  geom_point(aes(y = fertility)) +
  geom_line(aes(y = fitted))
               
png(file = .out,
    width = 400,
    height = 300)
plot(p)
dev.off()
