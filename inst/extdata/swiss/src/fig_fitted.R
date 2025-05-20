
suppressPackageStartupMessages({
  library(ggplot2)
  library(command)
})

cmd_assign(.vals_fitted = "out/vals_fitted.rds",
           .out = "out/fig_fitted.png")

vals_fitted <- readRDS(.vals_fitted)

p <- ggplot(vals_fitted, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0)
            
png(file = .out)
plot(p)
dev.off()
