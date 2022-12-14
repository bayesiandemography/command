
library(command)

cmd_assign(vals = "out/vals_fertility.rds",
           robust_method = "M")

pdf(file = "out/fig_fertility.pdf")

plot(fertility_actual ~ education, data = vals)
lines(fertility_modelled ~ education, data = vals)
title(paste("Fitted model, method =", robust_method))

dev.off()
